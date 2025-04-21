namespace Ballerina.DSL.FormEngine.Parser

module Renderers =
  open Model
  open Patterns
  open Expr
  open ExprType

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

  type Renderer with
    // static member ParseChildren(json: JsonValue) : State<RendererChildren, CodeGenConfig, ParsedFormsContext, Errors> =
    //   state {
    //     let! fieldsJson = json |> JsonValue.AsRecord |> state.OfSum

    //     let! parsedFields =
    //       fieldsJson
    //       |> Seq.map (fun (name, body) ->
    //         state {
    //           let! res = FieldConfig.Parse name body
    //           return name, res
    //         })
    //       |> state.All

    //     return { Fields = parsedFields |> Map.ofSeq }
    //   }

    static member Parse
      (parentJsonFields: (string * JsonValue)[])
      (json: JsonValue)
      : State<Renderer, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! config = state.GetContext()
        let! (formsState: ParsedFormsContext) = state.GetState()

        let! childrenJson =
          parentJsonFields
          |> sum.TryFindField "children"
          |> sum.Catch(fun () -> JsonValue.Record([||]))
          |> state.OfSum

        // let! children = Renderer.ParseChildren childrenJson
        let! s = json |> JsonValue.AsString |> state.OfSum

        if config.Bool.SupportedRenderers |> Set.contains s then
          return
            PrimitiveRenderer
              { PrimitiveRendererName = s
                PrimitiveRendererId = Guid.CreateVersion7()
                Type = ExprType.PrimitiveType PrimitiveType.BoolType }
        elif config.Date.SupportedRenderers |> Set.contains s then
          return
            PrimitiveRenderer
              { PrimitiveRendererName = s
                PrimitiveRendererId = Guid.CreateVersion7()
                Type = ExprType.PrimitiveType PrimitiveType.DateOnlyType }
        elif config.Unit.SupportedRenderers |> Set.contains s then
          return
            PrimitiveRenderer
              { PrimitiveRendererName = s
                PrimitiveRendererId = Guid.CreateVersion7()
                Type = ExprType.UnitType }
        elif config.Guid.SupportedRenderers |> Set.contains s then
          return
            PrimitiveRenderer
              { PrimitiveRendererName = s
                PrimitiveRendererId = Guid.CreateVersion7()
                Type = ExprType.PrimitiveType PrimitiveType.GuidType }
        elif config.Int.SupportedRenderers |> Set.contains s then
          return
            PrimitiveRenderer
              { PrimitiveRendererName = s
                PrimitiveRendererId = Guid.CreateVersion7()
                Type = ExprType.PrimitiveType PrimitiveType.IntType }
        elif config.String.SupportedRenderers |> Set.contains s then
          return
            PrimitiveRenderer
              { PrimitiveRendererName = s
                PrimitiveRendererId = Guid.CreateVersion7()
                Type = ExprType.PrimitiveType PrimitiveType.StringType }
        elif
          config.Option.SupportedRenderers.Enum |> Set.contains s
          || config.Set.SupportedRenderers.Enum |> Set.contains s
        then
          let containerTypeConstructor =
            if config.Option.SupportedRenderers.Enum |> Set.contains s then
              ExprType.OptionType
            else
              ExprType.SetType

          let! optionJson = parentJsonFields |> sum.TryFindField "options" |> state.OfSum
          let! enumName = optionJson |> JsonValue.AsString |> state.OfSum
          let! enum = formsState.TryFindEnum enumName |> state.OfSum
          let! enumType = formsState.TryFindType enum.TypeId.TypeName |> state.OfSum

          return
            EnumRenderer(
              enum |> EnumApi.Id,
              PrimitiveRenderer
                { PrimitiveRendererName = s
                  PrimitiveRendererId = Guid.CreateVersion7()
                  Type = containerTypeConstructor (enumType.Type) }
            )
        elif
          config.Option.SupportedRenderers.Stream |> Set.contains s
          || config.Set.SupportedRenderers.Stream |> Set.contains s
        then
          let containerTypeConstructor =
            if config.Option.SupportedRenderers.Stream |> Set.contains s then
              ExprType.OptionType
            else
              ExprType.SetType

          let! streamNameJson = parentJsonFields |> sum.TryFindField "stream" |> state.OfSum
          let! streamName = streamNameJson |> JsonValue.AsString |> state.OfSum
          let! stream = formsState.TryFindStream streamName |> state.OfSum
          let! streamType = formsState.TryFindType stream.TypeId.TypeName |> state.OfSum

          return
            StreamRenderer(
              stream |> StreamApi.Id,
              PrimitiveRenderer
                { PrimitiveRendererName = s
                  PrimitiveRendererId = Guid.CreateVersion7()
                  Type = containerTypeConstructor (streamType.Type) }
            )
        elif config.Map.SupportedRenderers |> Set.contains s then
          let! (keyRendererJson, valueRendererJson) =
            state.All2
              (parentJsonFields |> state.TryFindField "keyRenderer")
              (parentJsonFields |> state.TryFindField "valueRenderer")

          let! keyRenderer = NestedRenderer.Parse keyRendererJson
          let! valueRenderer = NestedRenderer.Parse valueRendererJson

          return
            MapRenderer
              {| Map =
                  PrimitiveRenderer
                    { PrimitiveRendererName = s
                      PrimitiveRendererId = Guid.CreateVersion7()
                      Type = ExprType.MapType(keyRenderer.Renderer.Type, valueRenderer.Renderer.Type) }
                 Key = keyRenderer
                 Value = valueRenderer |}
        elif config.Sum.SupportedRenderers |> Set.contains s then
          let! (leftRendererJson, rightRendererJson) =
            state.All2
              (parentJsonFields |> state.TryFindField "leftRenderer")
              (parentJsonFields |> state.TryFindField "rightRenderer")

          let! leftRenderer = NestedRenderer.Parse leftRendererJson
          let! rightRenderer = NestedRenderer.Parse rightRendererJson

          return
            SumRenderer
              {| Sum =
                  PrimitiveRenderer
                    { PrimitiveRendererName = s
                      PrimitiveRendererId = Guid.CreateVersion7()
                      Type = ExprType.SumType(leftRenderer.Renderer.Type, rightRenderer.Renderer.Type) }
                 Left = leftRenderer
                 Right = rightRenderer |}
        elif config.List.SupportedRenderers |> Set.contains s then
          let! elementRendererJson = parentJsonFields |> sum.TryFindField "elementRenderer" |> state.OfSum
          let! elementRenderer = NestedRenderer.Parse elementRendererJson

          return
            ListRenderer
              {| List =
                  PrimitiveRenderer
                    { PrimitiveRendererName = s
                      PrimitiveRendererId = Guid.CreateVersion7()
                      Type = ExprType.ListType elementRenderer.Renderer.Type }
                 Element = elementRenderer |}
        // elif config.Table.SupportedRenderers |> Set.contains s then
        //   let! elementRendererJson = parentJsonFields |> sum.TryFindField "rowRenderer" |> state.OfSum
        //   let! elementRenderer = NestedRenderer.Parse elementRendererJson

        //   return
        //     TableRenderer
        //       {| Table =
        //           PrimitiveRenderer
        //             { PrimitiveRendererName = s
        //               PrimitiveRendererId = Guid.CreateVersion7()
        //               Type = ExprType.TableType elementRenderer.Renderer.Type
        //               }
        //          Row = elementRenderer
        //          |}
        elif config.Union.SupportedRenderers |> Set.contains s then
          let! casesJson = parentJsonFields |> sum.TryFindField "cases" |> state.OfSum
          let! casesJson = casesJson |> JsonValue.AsRecord |> state.OfSum

          let! cases =
            casesJson
            |> Seq.map (fun (caseName, caseJson) ->
              state {
                let! caseRenderer = Renderer.Parse [||] caseJson
                return caseName, caseRenderer
              })
            |> state.All

          return
            UnionRenderer
              {| Union =
                  PrimitiveRenderer
                    { PrimitiveRendererName = s
                      PrimitiveRendererId = Guid.CreateVersion7()
                      Type =
                        ExprType.UnionType(
                          cases
                          |> Seq.map (fun (n, t) -> ({ CaseName = n }, { CaseName = n; Fields = t.Type }))
                          |> Map.ofSeq
                        ) }
                 Cases = cases |> Seq.map (fun (n, t) -> { CaseName = n }, t) |> Map.ofSeq |}
        else
          return!
            state.Any(
              NonEmptyList.OfList(
                state {
                  let! c =
                    config.Custom
                    |> Seq.tryFind (fun c -> c.Value.SupportedRenderers |> Set.contains s)
                    |> Sum.fromOption (fun () -> $"Error: cannot find custom type {s}" |> Errors.Singleton)
                    |> state.OfSum

                  let! t = formsState.TryFindType c.Key |> state.OfSum

                  return
                    PrimitiveRenderer
                      { PrimitiveRendererName = s
                        PrimitiveRendererId = Guid.CreateVersion7()
                        Type = t.Type }
                },
                [ state {
                    let! { GenericRenderers = genericRenderers } = state.GetState()

                    match genericRenderers with
                    | [] -> return! state.Throw(Errors.Singleton $"Error: cannot match empty generic renderers")
                    | g :: gs ->
                      let genericRenderers = NonEmptyList.OfList(g, gs)

                      return!
                        genericRenderers
                        |> NonEmptyList.map (fun g ->
                          state {
                            if g.SupportedRenderers |> Set.contains s then
                              return
                                PrimitiveRenderer
                                  { PrimitiveRendererName = s
                                    PrimitiveRendererId = Guid.CreateVersion7()
                                    Type = g.Type }

                            else
                              return! state.Throw(Errors.Singleton $"Error: generic renderer does not match")
                          })
                        |> state.Any
                  }
                  state {
                    let! tupleConfig =
                      config.Tuple
                      |> List.tryFind (fun t -> t.SupportedRenderers.Contains s)
                      |> Sum.fromOption (fun () -> Errors.Singleton $"Error: cannot find tuple config for renderer {s}")
                      |> state.OfSum

                    return!
                      state {
                        let! itemRenderersJson = parentJsonFields |> sum.TryFindField "itemRenderers" |> state.OfSum
                        let! itemRenderersJson = itemRenderersJson |> JsonValue.AsArray |> state.OfSum
                        let! itemRenderers = itemRenderersJson |> Seq.map (NestedRenderer.Parse) |> state.All

                        if itemRenderers.Length <> tupleConfig.Ariety then
                          return!
                            state.Throw(
                              Errors.Singleton
                                $"Error: mismatched tuple size. Expected {tupleConfig.Ariety}, found {itemRenderers.Length}."
                            )
                        else
                          return
                            TupleRenderer
                              {| Tuple =
                                  PrimitiveRenderer
                                    { PrimitiveRendererName = s
                                      PrimitiveRendererId = Guid.CreateVersion7()
                                      Type =
                                        ExprType.TupleType(itemRenderers |> Seq.map (fun nr -> nr.Type) |> List.ofSeq) }
                                 Elements = itemRenderers |}
                      }
                      |> state.MapError(Errors.WithPriority ErrorPriority.High)
                  }
                  state {
                    let! form = formsState.TryFindForm s |> state.OfSum

                    return!
                      state {
                        match form.Body with
                        | FormBody.Union cases ->
                          let formType = cases.UnionType
                          return FormRenderer(form |> FormConfig.Id, formType)
                        | FormBody.Record fields -> return FormRenderer(form |> FormConfig.Id, fields.RecordType)
                        | FormBody.Table table ->
                          // do Console.WriteLine form.FormName
                          // do Console.WriteLine parentJsonFields.ToFSharpString
                          // do Console.ReadLine() |> ignore
                          let! tableApiNameJson = parentJsonFields |> sum.TryFindField "api" |> state.OfSum
                          let! tableApiName = tableApiNameJson |> JsonValue.AsString |> state.OfSum
                          let! (tableApi) = formsState.TryFindTableApi tableApiName |> state.OfSum
                          let! tableType = formsState.TryFindType tableApi.TypeId.TypeName |> state.OfSum

                          return FormRenderer(form |> FormConfig.Id, tableType.Type |> ExprType.TableType)
                      }
                      |> state.MapError(Errors.WithPriority ErrorPriority.High)

                  }
                  state.Throw(
                    Errors.Singleton
                      $"Error: cannot resolve field renderer {s} in {(formsState.Forms |> Map.values |> Seq.map (fun v -> v.FormName) |> List.ofSeq).ToFSharpString}"
                    |> Errors.WithPriority ErrorPriority.High
                  ) ]
              )
            )
            |> state.MapError(Errors.HighestPriority)
      }
      |> state.WithErrorContext $"...when parsing renderer {json.ToString().ReasonablyClamped}"

  and NestedRenderer with
    static member Parse(json: JsonValue) : State<NestedRenderer, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! jsonFields = json |> JsonValue.AsRecord |> state.OfSum

        let! label =
          jsonFields
          |> sum.TryFindField "label"
          |> Sum.toOption
          |> Option.map (JsonValue.AsString >> state.OfSum)
          |> state.RunOption

        let! tooltip =
          jsonFields
          |> sum.TryFindField "tooltip"
          |> Sum.toOption
          |> Option.map (JsonValue.AsString >> state.OfSum)
          |> state.RunOption

        let! details =
          jsonFields
          |> sum.TryFindField "details"
          |> Sum.toOption
          |> Option.map (JsonValue.AsString >> state.OfSum)
          |> state.RunOption

        let! rendererJson = jsonFields |> state.TryFindField "renderer"
        let! renderer = Renderer.Parse jsonFields rendererJson

        return
          { Label = label
            Tooltip = tooltip
            Details = details
            Renderer = renderer }
      }
      |> state.WithErrorContext $"...when parsing renderer {json.ToString().ReasonablyClamped}"
