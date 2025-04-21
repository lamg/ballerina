namespace Ballerina.DSL.FormEngine.Parser

module Runner =
  open Model
  open Patterns
  open Expr
  open ExprType
  open Renderers

  open Ballerina.DSL.FormEngine.Model
  open Ballerina.DSL.Expr.Model
  open Ballerina.DSL.Expr.Patterns
  open Ballerina.DSL.Expr.Types.Model
  open Ballerina.DSL.Expr.Types.Patterns
  open System
  open Ballerina.Collections.Sum
  open Ballerina.Collections.Map
  open Ballerina.State.WithError
  open Ballerina.Errors
  open Ballerina.Core.Json
  open Ballerina.Core.String
  open Ballerina.Core.Object
  open FSharp.Data
  open Ballerina.Collections.NonEmptyList

  type FieldConfig with
    static member Parse
      (fieldName: string)
      (json: JsonValue)
      : State<FieldConfig, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! fields = json |> JsonValue.AsRecord |> state.OfSum

        let! label =
          fields
          |> sum.TryFindField "label"
          |> Sum.toOption
          |> Option.map (JsonValue.AsString >> state.OfSum)
          |> state.RunOption

        let! tooltip =
          fields
          |> sum.TryFindField "tooltip"
          |> Sum.toOption
          |> Option.map (JsonValue.AsString >> state.OfSum)
          |> state.RunOption

        let! details =
          fields
          |> sum.TryFindField "details"
          |> Sum.toOption
          |> Option.map (JsonValue.AsString >> state.OfSum)
          |> state.RunOption

        let! rendererJson, visibleJson =
          state.All2 (fields |> state.TryFindField "renderer") (fields |> state.TryFindField "visible" |> state.Catch)

        let! disabledJson = sum.TryFindField "disabled" fields |> state.OfSum |> state.Catch
        let! renderer = Renderer.Parse fields rendererJson
        let! visible = visibleJson |> Sum.toOption |> Option.map Expr.Parse |> state.RunOption

        let visible =
          visible |> Option.defaultWith (fun () -> Expr.Value(Value.ConstBool true))

        let! disabled = disabledJson |> Sum.toOption |> Option.map (Expr.Parse) |> state.RunOption

        let fc =
          { FieldName = fieldName
            FieldId = Guid.CreateVersion7()
            Label = label
            Tooltip = tooltip
            Details = details
            Renderer = renderer
            Visible = visible
            Disabled = disabled }

        return fc
      }
      |> state.WithErrorContext $"...when parsing field {fieldName}"

  type FormLauncher with
    static member Parse (launcherName: string) (json: JsonValue) : State<_, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! launcherFields = JsonValue.AsRecord json |> state.OfSum

        let! kindJson, formNameJson =
          state.All2 (launcherFields |> state.TryFindField "kind") (launcherFields |> state.TryFindField "form")
        // (launcherFields |> state.TryFindField "api")
        // (launcherFields |> state.TryFindField "configApi")
        let! (kind, formName) =
          state.All2 (JsonValue.AsString kindJson |> state.OfSum) (JsonValue.AsString formNameJson |> state.OfSum)

        let! (s: ParsedFormsContext) = state.GetState()
        let! form = s.TryFindForm formName |> state.OfSum

        if kind = "create" || kind = "edit" then
          let! entityApiJson, configApiJson =
            state.All2 (launcherFields |> state.TryFindField "api") (launcherFields |> state.TryFindField "configApi")

          let! entityApiName, configApiName =
            state.All2
              (entityApiJson |> JsonValue.AsString |> state.OfSum)
              (configApiJson |> JsonValue.AsString |> state.OfSum)

          let! api = s.TryFindEntityApi entityApiName |> state.OfSum
          let! configApi = s.TryFindEntityApi configApiName |> state.OfSum
          let api, configApi = api |> fst |> EntityApi.Id, configApi |> fst |> EntityApi.Id

          return
            (if kind = "create" then
               FormLauncherMode.Create
                 { EntityApi = api
                   ConfigEntityApi = configApi }
             else
               FormLauncherMode.Edit
                 { EntityApi = api
                   ConfigEntityApi = configApi }),
            form |> FormConfig.Id
        elif kind = "passthrough" then
          let! configTypeJson = launcherFields |> state.TryFindField "configType"
          let! configTypeName = configTypeJson |> JsonValue.AsString |> state.OfSum
          let! configType = state.TryFindType configTypeName
          return FormLauncherMode.Passthrough {| ConfigType = configType.TypeId |}, form |> FormConfig.Id
        elif kind = "passthrough-table" then
          let! configTypeJson = launcherFields |> state.TryFindField "configType"
          let! configTypeName = configTypeJson |> JsonValue.AsString |> state.OfSum
          let! configType = state.TryFindType configTypeName
          let! apiNameJson = launcherFields |> state.TryFindField "api"
          let! apiName = apiNameJson |> JsonValue.AsString |> state.OfSum
          let! api = s.TryFindTableApi apiName |> state.OfSum

          return
            FormLauncherMode.PassthroughTable
              {| ConfigType = configType.TypeId
                 TableApi = api |> TableApi.Id |},
            form |> FormConfig.Id
        else
          return!
            $"Error: invalid launcher mode {kind}: it should be either 'create' or 'edit'."
            |> Errors.Singleton
            |> state.Throw
      }
      |> state.WithErrorContext $"...when parsing launcher {launcherName}"

  type FormFields with
    static member Parse(fields: (string * JsonValue)[]) =
      state {
        let! fieldsJson, tabsJson =
          state.All2 (fields |> state.TryFindField "fields") (fields |> state.TryFindField "tabs")

        let! extendsJson = fields |> state.TryFindField "extends" |> state.Catch |> state.Map Sum.toOption

        let! extendedForms =
          extendsJson
          |> Option.map (fun extendsJson ->
            state {
              let! extendsJson = extendsJson |> JsonValue.AsArray |> state.OfSum

              return!
                extendsJson
                |> Seq.map (fun extendJson ->
                  state {
                    let! extendsFormName = extendJson |> JsonValue.AsString |> state.OfSum
                    return! state.TryFindForm extendsFormName
                  })
                |> state.All
            })
          |> state.RunOption

        let! extendedFields =
          match extendedForms with
          | None -> state.Return []
          | Some fs ->
            fs
            |> Seq.map (fun f -> FormBody.TryGetFields f.Body)
            |> state.All
            |> state.Map(List.map (fun f -> f.Fields.Fields))

        let! formFields = fieldsJson |> JsonValue.AsRecord |> state.OfSum

        let! fieldConfigs =
          formFields
          |> Seq.map (fun (fieldName, fieldJson) ->
            state {
              let! parsedField = FieldConfig.Parse fieldName fieldJson
              return fieldName, parsedField
            })
          |> state.All

        let fieldConfigs = fieldConfigs |> Map.ofSeq
        let fieldConfigs = Map.mergeMany (fun x y -> x) (fieldConfigs :: extendedFields)
        let! tabs = FormConfig.ParseTabs fieldConfigs tabsJson

        return
          { FormFields.Fields = fieldConfigs
            FormFields.Tabs = tabs }
      }

  and FormBody with
    static member Parse (fields: (string * JsonValue)[]) (formTypeId: TypeId) =
      state.Either3
        (state {
          let! formFields = FormFields.Parse fields
          let! t = state.TryFindType formTypeId.TypeName

          return
            FormBody.Record
              {| Fields = formFields
                 RecordType = t.Type |}
        })
        (state {
          let! casesJson = fields |> state.TryFindField "cases"

          return!
            state {
              let! casesJson = casesJson |> JsonValue.AsRecord |> state.OfSum
              let! rendererJson = fields |> state.TryFindField "renderer"
              let! renderer = Renderer.Parse fields rendererJson
              let! t = state.TryFindType formTypeId.TypeName

              let! cases =
                casesJson
                |> Seq.map (fun (caseName, caseJson) ->
                  state {
                    // let! caseJson = caseJson |> JsonValue.AsRecord |> state.OfSum
                    let! caseBody = Renderer.Parse [||] caseJson
                    return caseName, caseBody
                  }
                  |> state.MapError(Errors.Map(String.appendNewline $"\n...when parsing form case {caseName}")))
                |> state.All
                |> state.Map(Map.ofSeq)

              return
                {| Cases = cases
                   Renderer = renderer
                   UnionType = t.Type |}
                |> FormBody.Union
            }
            |> state.MapError(Errors.WithPriority ErrorPriority.High)
        })
        (state {
          let! columnsJson = fields |> state.TryFindField "columns"

          return!
            state {
              let! columnsJson = columnsJson |> JsonValue.AsRecord |> state.OfSum
              let! rendererJson = fields |> state.TryFindField "renderer"
              let! renderer = rendererJson |> JsonValue.AsString |> state.OfSum
              let! config = state.GetContext()
              let! t = state.TryFindType formTypeId.TypeName

              if config.Table.SupportedRenderers |> Set.contains renderer |> not then
                return! state.Throw(Errors.Singleton $"Error: cannot find table renderer {renderer}")
              else
                let! columns =
                  columnsJson
                  |> Seq.map (fun (columnName, columnJson) ->
                    state {
                      // let! caseJson = caseJson |> JsonValue.AsRecord |> state.OfSum
                      let! columnBody = FieldConfig.Parse columnName columnJson
                      return columnName, columnBody
                    }
                    |> state.MapError(Errors.Map(String.appendNewline $"\n...when parsing table column {columnName}")))
                  |> state.All
                  |> state.Map(Map.ofSeq)

                let! visibleColumnsJson = fields |> state.TryFindField "visibleColumns"
                let! visibleColumns = FormConfig.ParseGroup "visibleColumns" columns visibleColumnsJson

                return
                  {| Columns = columns
                     RowType = t.Type
                     Renderer = renderer
                     VisibleColumns = visibleColumns |}
                  |> FormBody.Table
            }
            |> state.MapError(Errors.WithPriority ErrorPriority.High)
        })

  and FormConfig with
    static member ParseGroup
      (groupName: string)
      (fieldConfigs: Map<string, FieldConfig>)
      (json: JsonValue)
      : State<FormGroup, CodeGenConfig, ParsedFormsContext, Errors> =
      state.Either
        (state {
          let! fields = json |> JsonValue.AsArray |> state.OfSum

          return!
            seq {
              for fieldJson in fields do
                yield
                  state {
                    let! fieldName = fieldJson |> JsonValue.AsString |> state.OfSum

                    return!
                      fieldConfigs
                      |> Map.tryFindWithError fieldName "field name" fieldName
                      |> Sum.map (FieldConfig.Id)
                      |> state.OfSum
                  }
            }
            |> state.All
            |> state.Map(FormGroup.Inlined)
            |> state.MapError(Errors.WithPriority ErrorPriority.High)
        })
        (state {
          let! expr = json |> Expr.Parse
          return FormGroup.Computed expr
        })
      |> state.WithErrorContext $"...when parsing group {groupName}"

    static member ParseColumn
      (columnName: string)
      fieldConfigs
      (json: JsonValue)
      : State<FormGroups, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! jsonFields = json |> JsonValue.AsRecord |> state.OfSum

        match jsonFields with
        | [| "groups", JsonValue.Record groups |] ->
          let! groups =
            seq {
              for groupName, groupJson in groups do
                yield
                  state {
                    let! column = FormConfig.ParseGroup groupName fieldConfigs groupJson
                    return groupName, column
                  }
            }
            |> state.All
            |> state.Map Map.ofList

          return { FormGroups = groups }
        | _ ->
          return!
            $"Error: cannot parse groups. Expected a single field 'groups', instead found {json}"
            |> Errors.Singleton
            |> state.Throw
      }
      |> state.WithErrorContext $"...when parsing column {columnName}"

    static member ParseTab
      (tabName: string)
      fieldConfigs
      (json: JsonValue)
      : State<FormColumns, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! jsonFields = json |> JsonValue.AsRecord |> state.OfSum

        match jsonFields with
        | [| "columns", JsonValue.Record columns |] ->
          let! columns =
            seq {
              for columnName, columnJson in columns do
                yield
                  state {
                    let! column = FormConfig.ParseColumn columnName fieldConfigs columnJson
                    return columnName, column
                  }
            }
            |> state.All
            |> state.Map Map.ofList

          return { FormColumns = columns }
        | _ ->
          return!
            $"Error: cannot parse columns. Expected a single field 'columns', instead found {json}"
            |> Errors.Singleton
            |> state.Throw
      }
      |> state.WithErrorContext $"...when parsing tab {tabName}"

    static member ParseTabs
      fieldConfigs
      (json: JsonValue)
      : State<FormTabs, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! tabs = json |> JsonValue.AsRecord |> state.OfSum

        let! tabs =
          seq {
            for tabName, tabJson in tabs do
              yield
                state {
                  let! column = FormConfig.ParseTab tabName fieldConfigs tabJson
                  return tabName, column
                }
          }
          |> state.All
          |> state.Map Map.ofList

        return { FormTabs = tabs }
      }
      |> state.WithErrorContext $"...when parsing tabs"

    static member PreParse
      (formName: string)
      (json: JsonValue)
      : State<TypeBinding, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! fields = json |> JsonValue.AsRecord |> state.OfSum

        let! typeJson = (fields |> state.TryFindField "type")
        let! typeName = typeJson |> JsonValue.AsString |> state.OfSum
        let! (s: ParsedFormsContext) = state.GetState()
        let! typeBinding = s.TryFindType typeName |> state.OfSum

        return typeBinding
      }

    static member Parse
      (formName: string)
      (json: JsonValue)
      : State<{| TypeId: TypeId; Body: FormBody |}, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! fields = json |> JsonValue.AsRecord |> state.OfSum

        let! typeJson = (fields |> state.TryFindField "type")
        let! typeName = typeJson |> JsonValue.AsString |> state.OfSum
        let! (s: ParsedFormsContext) = state.GetState()
        let! typeBinding = s.TryFindType typeName |> state.OfSum
        let! body = FormBody.Parse fields typeBinding.TypeId

        return
          {| TypeId = typeBinding.TypeId
             Body = body |}
      }
      |> state.WithErrorContext $"...when parsing form {formName}"

  type EnumApi with
    static member Parse
      valueFieldName
      (enumName: string)
      (enumTypeJson: JsonValue)
      : State<Unit, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! enumType = ExprType.Parse enumTypeJson
        let! enumTypeId = enumType |> ExprType.AsLookupId |> state.OfSum
        let! ctx = state.GetState()
        let! enumType = ExprType.ResolveLookup ctx enumType |> state.OfSum
        let! fields = ExprType.GetFields enumType |> state.OfSum

        match fields with
        | [ (value, ExprType.LookupType underlyingUnion) ] when value = valueFieldName ->
          do!
            state.SetState(
              ParsedFormsContext.Updaters.Apis(
                FormApis.Updaters.Enums(
                  Map.add
                    enumName
                    { EnumApi.TypeId = enumTypeId
                      EnumName = enumName
                      UnderlyingEnum = underlyingUnion }
                )
              )
            )
        | _ ->
          return!
            state.Throw(
              $$"""Error: invalid enum reference type passed to enum '{{enumName}}'. Expected { {{valueFieldName}}:ENUM }, found {{fields}}."""
              |> Errors.Singleton
            )
      }

  type StreamApi with
    static member Parse
      (streamName: string)
      (streamTypeJson: JsonValue)
      : State<Unit, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! streamType = ExprType.Parse streamTypeJson
        let! streamTypeId = streamType |> ExprType.AsLookupId |> state.OfSum

        do!
          state.SetState(
            ParsedFormsContext.Updaters.Apis(
              FormApis.Updaters.Streams(
                Map.add
                  streamName
                  { StreamApi.TypeId = streamTypeId
                    StreamName = streamName }
              )
            )
          )
      }
      |> state.WithErrorContext $"...when parsing stream {streamName}"

  type CrudMethod with
    static member Parse(crudMethodJson: JsonValue) : State<CrudMethod, CodeGenConfig, ParsedFormsContext, Errors> =
      let crudCase name value =
        state {
          do!
            crudMethodJson
            |> JsonValue.AsEnum(Set.singleton name)
            |> state.OfSum
            |> state.Map ignore

          return value
        }

      state.Any(
        NonEmptyList.OfList(
          crudCase "create" CrudMethod.Create,
          [ crudCase "get" CrudMethod.Get
            crudCase "update" CrudMethod.Update
            crudCase "default" CrudMethod.Default ]
        )
      )

  type TableApi with
    static member Parse
      (tableName: string)
      (tableTypeJson: JsonValue)
      : State<TableApi, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! tableTypeFieldJsons = tableTypeJson |> JsonValue.AsRecord |> state.OfSum

        let! typeJson = (tableTypeFieldJsons |> state.TryFindField "type")

        let! tableType = ExprType.Parse typeJson
        let! tableTypeId = tableType |> ExprType.AsLookupId |> state.OfSum

        let tableApi =
          ({ TableApi.TypeId = tableTypeId
             TableName = tableName })

        return tableApi
      }
      |> state.WithErrorContext $"...when parsing table api {tableName}"

  type EntityApi with
    static member Parse
      (entityName: string)
      (entityTypeJson: JsonValue)
      : State<EntityApi * Set<CrudMethod>, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! entityTypeFieldJsons = entityTypeJson |> JsonValue.AsRecord |> state.OfSum

        let! typeJson, methodsJson =
          state.All2
            (entityTypeFieldJsons |> state.TryFindField "type")
            (entityTypeFieldJsons |> state.TryFindField "methods")

        let! methodsJson = methodsJson |> JsonValue.AsArray |> state.OfSum
        let! entityType = ExprType.Parse typeJson
        let! entityTypeId = entityType |> ExprType.AsLookupId |> state.OfSum
        let! methods = methodsJson |> Seq.map CrudMethod.Parse |> state.All |> state.Map Set.ofSeq

        let entityApi =
          { EntityApi.TypeId = entityTypeId
            EntityName = entityName },
          methods

        return entityApi
      }
      |> state.WithErrorContext $"...when parsing entity api {entityName}"

  type LookupApi with
    static member Parse
      (parentEntityName: string)
      (lookupApiJson: JsonValue)
      : State<LookupApi, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! lookupApiFields = lookupApiJson |> JsonValue.AsRecord |> state.OfSum

        let! onesJson, manysJson =
          state.All2
            (state.Either (lookupApiFields |> state.TryFindField "one") (state.Return(JsonValue.Record [||])))
            (state.Either (lookupApiFields |> state.TryFindField "many") (state.Return(JsonValue.Record [||])))

        let! onesFields, manysFields =
          state.All2 (onesJson |> JsonValue.AsRecord |> state.OfSum) (manysJson |> JsonValue.AsRecord |> state.OfSum)

        let! ones =
          state.All(
            onesFields
            |> Seq.map (fun (entityName, entityJson) ->
              state {
                let! entityApi = EntityApi.Parse entityName entityJson
                return entityName, entityApi
              })
          )
          |> state.Map(Map.ofSeq)

        let! manys =
          state.All(
            manysFields
            |> Seq.map (fun (tableName, tableJson) ->
              state {
                let! tableApi = TableApi.Parse tableName tableJson
                return tableName, tableApi
              })
          )
          |> state.Map(Map.ofSeq)

        let lookupApi =
          { LookupApi.EntityName = parentEntityName
            Enums = Map.empty
            Streams = Map.empty
            Ones = ones
            Manys = manys }

        return lookupApi
      }

  type ParsedFormsContext with
    static member ParseApis
      enumValueFieldName
      (topLevel: TopLevel)
      : State<Unit, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let enums, streams, entities, tables, lookups =
          topLevel.Enums, topLevel.Streams, topLevel.Entities, topLevel.Tables, topLevel.Lookups

        for enumName, enumJson in enums do
          do! EnumApi.Parse enumValueFieldName enumName enumJson

        for streamName, streamJson in streams do
          do! StreamApi.Parse streamName streamJson

        for entityName, entityJson in entities do
          let! entityApi = EntityApi.Parse entityName entityJson
          do! state.SetState(ParsedFormsContext.Updaters.Apis(FormApis.Updaters.Entities(Map.add entityName entityApi)))

        for tableName, tableJson in tables do
          let! tableApi = TableApi.Parse tableName tableJson
          do! state.SetState(ParsedFormsContext.Updaters.Apis(FormApis.Updaters.Tables(Map.add tableName tableApi)))

        for lookupName, lookupJson in lookups do
          let! lookupApi = LookupApi.Parse lookupName lookupJson
          do Console.WriteLine (lookupName, lookupJson, lookupApi).ToFSharpString
          do Console.ReadLine() |> ignore
          do! state.SetState(ParsedFormsContext.Updaters.Apis(FormApis.Updaters.Lookups(Map.add lookupName lookupApi)))

        return ()
      }
      |> state.MapError(Errors.Map(String.appendNewline $$"""...when parsing APIs"""))

    static member ParseTypes
      (typesJson: seq<string * JsonValue>)
      : State<Unit, CodeGenConfig, ParsedFormsContext, Errors> =
      state {

        let! typesJson =
          typesJson
          |> Seq.map (fun (name, json) ->
            state {
              let typeId: TypeId = { TypeName = name }

              do!
                state.SetState(
                  ParsedFormsContext.Updaters.Types(
                    Map.add
                      name
                      { Type = ExprType.UnitType
                        TypeId = typeId
                        Const = false }
                  )
                )

              return name, typeId, json
            })
          |> state.All

        for typeName, typeId, typeJson in typesJson do
          return!
            state {
              let! typeJsonArgs = typeJson |> JsonValue.AsRecord |> state.OfSum

              return!
                state.Any(
                  NonEmptyList.OfList(
                    state {
                      let extendsJson =
                        typeJsonArgs
                        |> sum.TryFindField "extends"
                        |> Sum.toOption
                        |> Option.defaultWith (fun () -> JsonValue.Array [||])

                      let isConstJson =
                        typeJsonArgs
                        |> sum.TryFindField "const"
                        |> Sum.toOption
                        |> Option.defaultWith (fun () -> JsonValue.Boolean false)

                      let! fieldsJson = typeJsonArgs |> sum.TryFindField "fields" |> state.OfSum

                      return!
                        state {
                          let! extends, fields, isConst =
                            state.All3
                              (extendsJson |> JsonValue.AsArray |> state.OfSum)
                              (fieldsJson |> JsonValue.AsRecord |> state.OfSum)
                              (isConstJson |> JsonValue.AsBoolean |> state.OfSum)

                          let! s = state.GetState()

                          let! extendedTypes =
                            extends
                            |> Seq.map (fun extendsJson ->
                              state {
                                let! parsed = ExprType.Parse extendsJson
                                return! ExprType.ResolveLookup s parsed |> state.OfSum
                              })
                            |> state.All

                          let! fields =
                            fields
                            |> Seq.map (fun (fieldName, fieldType) ->
                              state {
                                let! fieldType = ExprType.Parse fieldType
                                return fieldName, fieldType
                              }
                              |> state.MapError(
                                Errors.Map(String.appendNewline $"\n...when parsing field {fieldName}")
                              ))
                            |> Seq.toList
                            |> state.All

                          let fields = fields |> Map.ofList

                          let! exprType =
                            extendedTypes
                            |> Seq.fold
                              (fun (t1: Sum<ExprType, Errors>) t2 ->
                                sum {
                                  let! t1 = t1
                                  return! ExprType.Extend t1 t2
                                })
                              (Left(ExprType.RecordType fields))
                            |> state.OfSum

                          do!
                            state.SetState(
                              ParsedFormsContext.Updaters.Types(
                                Map.add
                                  typeName
                                  { Type = exprType
                                    TypeId = typeId
                                    Const = isConst }
                              )
                            )

                          return ()
                        }
                        |> state.MapError(Errors.WithPriority ErrorPriority.High)
                    },
                    [ state {
                        let typeId: TypeId = { TypeName = typeName }

                        let! parsedType = ExprType.Parse typeJson

                        do!
                          state.SetState(
                            ParsedFormsContext.Updaters.Types(
                              Map.add
                                typeName
                                { Type = parsedType
                                  TypeId = typeId
                                  Const = false }
                            )
                          )
                      }
                      state.Throw(
                        Errors.Singleton
                          $"...unexpected json shape for a type body {typeJson.ToFSharpString.ReasonablyClamped}"
                        |> Errors.WithPriority ErrorPriority.High
                      ) ]
                  )
                )
            }
            |> state.MapError(Errors.Map(String.appendNewline $"\n...when parsing type {typeName}"))
      }
      |> state.MapError(Errors.Map(String.appendNewline $"\n...when parsing types"))

    static member ParseForms
      (formsJson: (string * JsonValue)[])
      : State<Unit, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        for formName, formJson in formsJson do
          let! formType = FormConfig.PreParse formName formJson

          do!
            state.SetState(
              ParsedFormsContext.Updaters.Forms(
                Map.add
                  formName
                  { Body =
                      FormBody.Union
                        {| Renderer =
                            Renderer.PrimitiveRenderer
                              { PrimitiveRendererName = ""
                                PrimitiveRendererId = Guid.CreateVersion7()
                                Type = ExprType.UnitType }
                           Cases = Map.empty
                           UnionType = formType.Type |}
                    FormId = Guid.CreateVersion7()
                    FormName = formName }
              )
            )

        for formName, formJson in formsJson do
          let! formBody = FormConfig.Parse formName formJson
          let! form = state.TryFindForm formName

          do! state.SetState(ParsedFormsContext.Updaters.Forms(Map.add formName { form with Body = formBody.Body }))
      }
      |> state.WithErrorContext $"...when parsing forms"

    static member ParseLaunchers
      (launchersJson: (string * JsonValue)[])
      : State<Unit, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        for launcherName, launcherJson in launchersJson do
          let! (mode, formId) = FormLauncher.Parse launcherName launcherJson

          do!
            state.SetState(
              ParsedFormsContext.Updaters.Launchers(
                Map.add
                  launcherName
                  { LauncherName = launcherName
                    LauncherId = Guid.CreateVersion7()
                    Mode = mode
                    Form = formId }
              )
            )
      }
      |> state.WithErrorContext $"...when parsing launchers"

    static member ExtractTopLevel json =
      state {
        let! properties = json |> JsonValue.AsRecord |> state.OfSum

        let! typesJson, apisJson, formsJson, launchersJson =
          state.All4
            (state.Either (properties |> state.TryFindField "types") (state.Return(JsonValue.Record [||])))
            (state.Either (properties |> state.TryFindField "apis") (state.Return(JsonValue.Record [||])))
            (state.Either (properties |> state.TryFindField "forms") (state.Return(JsonValue.Record [||])))
            (state.Either (properties |> state.TryFindField "launchers") (state.Return(JsonValue.Record [||])))

        let! typesJson, apisJson, formsJson, launchersJson =
          state.All4
            (typesJson |> JsonValue.AsRecord |> state.OfSum)
            (apisJson |> JsonValue.AsRecord |> state.OfSum)
            (formsJson |> JsonValue.AsRecord |> state.OfSum)
            (launchersJson |> JsonValue.AsRecord |> state.OfSum)

        let! enumsJson, searchableStreamsJson, entitiesJson, tablesJson, lookupsJson =
          state.All5
            (state.Either (apisJson |> state.TryFindField "enumOptions") (state.Return(JsonValue.Record [||])))
            (state.Either (apisJson |> state.TryFindField "searchableStreams") (state.Return(JsonValue.Record [||])))
            (state.Either (apisJson |> state.TryFindField "entities") (state.Return(JsonValue.Record [||])))
            (state.Either (apisJson |> state.TryFindField "tables") (state.Return(JsonValue.Record [||])))
            (state.Either (apisJson |> state.TryFindField "lookups") (state.Return(JsonValue.Record [||])))

        let! enums, streams, entities, tables, lookups =
          state.All5
            (enumsJson |> JsonValue.AsRecord |> state.OfSum)
            (searchableStreamsJson |> JsonValue.AsRecord |> state.OfSum)
            (entitiesJson |> JsonValue.AsRecord |> state.OfSum)
            (tablesJson |> JsonValue.AsRecord |> state.OfSum)
            (lookupsJson |> JsonValue.AsRecord |> state.OfSum)

        return
          { Types = typesJson
            Forms = formsJson
            Launchers = launchersJson
            Enums = enums
            Streams = streams
            Entities = entities
            Tables = tables
            Lookups = lookups }
      }

    static member Parse
      generatedLanguageSpecificConfig
      (jsons: List<JsonValue>)
      : State<TopLevel, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        // let! ctx = state.GetState()
        // do System.Console.WriteLine ctx.Types.ToFSharpString
        // do System.Console.ReadLine() |> ignore
        let! topLevel = jsons |> List.map ParsedFormsContext.ExtractTopLevel |> state.All
        let! topLevel = TopLevel.MergeMany topLevel |> state.OfSum

        do! ParsedFormsContext.ParseTypes topLevel.Types
        let! c = state.GetContext()

        for g in c.Generic do
          let tstring = g.Type

          let! tjson =
            JsonValue.TryParse tstring
            |> Sum.fromOption (fun () -> Errors.Singleton $"Error: cannot parse generic type {tstring}")
            |> state.OfSum

          let! t = ExprType.Parse tjson

          do!
            state.SetState(
              ParsedFormsContext.Updaters.GenericRenderers(fun l ->
                {| Type = t
                   SupportedRenderers = g.SupportedRenderers |}
                :: l)
            )

        let! s = state.GetState()
        do! ParsedFormsContext.ParseApis generatedLanguageSpecificConfig.EnumValueFieldName topLevel
        do! ParsedFormsContext.ParseForms topLevel.Forms
        do! ParsedFormsContext.ParseLaunchers topLevel.Launchers
        return topLevel
      }
