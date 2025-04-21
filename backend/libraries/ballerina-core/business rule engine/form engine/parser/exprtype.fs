namespace Ballerina.DSL.FormEngine.Parser

module ExprType =
  open Model
  open Patterns

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

  type ExprType with
    static member ParseUnionCase(json: JsonValue) : State<UnionCase, CodeGenConfig, ParsedFormsContext, Errors> =
      state {
        let! args = json |> JsonValue.AsRecord |> state.OfSum

        let! caseJson, fieldsJson =
          state.All2 (args |> state.TryFindField "caseName") (args |> state.TryFindField "fields")

        let! caseName = caseJson |> JsonValue.AsString |> state.OfSum

        let! fieldsType =
          state.Either
            (state {
              let! fieldsJson = fieldsJson |> JsonValue.AsRecord |> state.OfSum

              let! fields =
                fieldsJson
                |> Seq.map (fun (fieldName, fieldType) ->
                  state {
                    let! fieldType = ExprType.Parse fieldType
                    return fieldName, fieldType
                  }
                  |> state.MapError(Errors.Map(String.appendNewline $"\n...when parsing field {fieldName}")))
                |> Seq.toList
                |> state.All

              let fields = fields |> Map.ofList

              if fields |> Map.isEmpty then
                ExprType.UnitType
              else
                ExprType.RecordType fields
            })
            (ExprType.Parse fieldsJson)

        return
          { CaseName = caseName
            Fields = fieldsType }
      }

    static member Parse(json: JsonValue) : State<ExprType, CodeGenConfig, ParsedFormsContext, Errors> =
      let (!) = ExprType.Parse

      state {
        return!
          state.Any(
            NonEmptyList.OfList(
              state {
                do!
                  json
                  |> JsonValue.AsEnum(Set.singleton "unit")
                  |> state.OfSum
                  |> state.Map ignore

                return ExprType.UnitType
              },
              [ state {
                  do!
                    json
                    |> JsonValue.AsEnum(Set.singleton "guid")
                    |> state.OfSum
                    |> state.Map ignore

                  return ExprType.PrimitiveType PrimitiveType.GuidType
                }
                state {
                  do!
                    json
                    |> JsonValue.AsEnum(Set.singleton "string")
                    |> state.OfSum
                    |> state.Map ignore

                  return ExprType.PrimitiveType PrimitiveType.StringType
                }
                state {
                  do!
                    json
                    |> JsonValue.AsEnum(Set.singleton "number")
                    |> state.OfSum
                    |> state.Map ignore

                  return ExprType.PrimitiveType PrimitiveType.IntType
                }
                state {
                  do!
                    json
                    |> JsonValue.AsEnum(Set.singleton "boolean")
                    |> state.OfSum
                    |> state.Map ignore

                  return ExprType.PrimitiveType PrimitiveType.BoolType
                }
                state {
                  do!
                    json
                    |> JsonValue.AsEnum(Set.singleton "Date")
                    |> state.OfSum
                    |> state.Map ignore

                  return ExprType.PrimitiveType PrimitiveType.DateOnlyType
                }
                state {
                  let! typeName = json |> JsonValue.AsString |> state.OfSum

                  return!
                    state {
                      let! (s: ParsedFormsContext) = state.GetState()
                      let! typeId = s.TryFindType typeName |> state.OfSum
                      return ExprType.LookupType typeId.TypeId
                    }
                    |> state.MapError(Errors.WithPriority ErrorPriority.High)
                }
                state {
                  let! fields = json |> JsonValue.AsRecord |> state.OfSum
                  let! funJson = (fields |> state.TryFindField "fun")

                  return!
                    state.Any(
                      NonEmptyList.OfList(
                        state {
                          do!
                            funJson
                            |> JsonValue.AsEnum(Set.singleton "SingleSelection")
                            |> state.OfSum
                            |> state.Map(ignore)

                          return!
                            state {
                              let! argsJson = (fields |> state.TryFindField "args")
                              let! arg = JsonValue.AsSingleton argsJson |> state.OfSum
                              let! arg = !arg
                              return ExprType.OptionType arg
                            }
                            |> state.MapError(Errors.WithPriority ErrorPriority.High)
                        },
                        [ state {
                            do!
                              funJson
                              |> JsonValue.AsEnum(Set.singleton "Option")
                              |> state.OfSum
                              |> state.Map(ignore)

                            return!
                              state {
                                let! argsJson = (fields |> state.TryFindField "args")
                                let! arg = JsonValue.AsSingleton argsJson |> state.OfSum
                                let! arg = !arg
                                return ExprType.OptionType arg
                              }
                              |> state.MapError(Errors.WithPriority ErrorPriority.High)
                          }
                          state {
                            do!
                              funJson
                              |> JsonValue.AsEnum(Set.singleton "One")
                              |> state.OfSum
                              |> state.Map(ignore)

                            return!
                              state {
                                let! argsJson = (fields |> state.TryFindField "args")
                                let! arg = JsonValue.AsSingleton argsJson |> state.OfSum
                                let! arg = !arg
                                return ExprType.OneType arg
                              }
                              |> state.MapError(Errors.WithPriority ErrorPriority.High)
                          }
                          state {
                            do!
                              funJson
                              |> JsonValue.AsEnum(Set.singleton "Many")
                              |> state.OfSum
                              |> state.Map(ignore)

                            return!
                              state {
                                let! argsJson = (fields |> state.TryFindField "args")
                                let! arg = JsonValue.AsSingleton argsJson |> state.OfSum
                                let! arg = !arg
                                return ExprType.ManyType arg
                              }
                              |> state.MapError(Errors.WithPriority ErrorPriority.High)
                          }
                          state {
                            do!
                              funJson
                              |> JsonValue.AsEnum(Set.singleton "MultiSelection")
                              |> state.OfSum
                              |> state.Map(ignore)

                            return!
                              state {
                                let! argsJson = (fields |> state.TryFindField "args")
                                let! arg = JsonValue.AsSingleton argsJson |> state.OfSum
                                let! arg = !arg
                                return ExprType.SetType arg
                              }
                              |> state.MapError(Errors.WithPriority ErrorPriority.High)
                          }
                          state {
                            do!
                              funJson
                              |> JsonValue.AsEnum(Set.singleton "Tuple")
                              |> state.OfSum
                              |> state.Map(ignore)

                            return!
                              state {
                                let! argsJson = (fields |> state.TryFindField "args")
                                let! args = JsonValue.AsArray argsJson |> state.OfSum
                                let! args = args |> Seq.map (!) |> state.All
                                return ExprType.TupleType args
                              }
                              |> state.MapError(Errors.WithPriority ErrorPriority.High)
                          }
                          state {
                            do!
                              funJson
                              |> JsonValue.AsEnum(Set.singleton "List")
                              |> state.OfSum
                              |> state.Map(ignore)

                            return!
                              state {
                                let! argsJson = (fields |> state.TryFindField "args")
                                let! arg = JsonValue.AsSingleton argsJson |> state.OfSum
                                let! arg = !arg
                                return ExprType.ListType arg
                              }
                              |> state.MapError(Errors.WithPriority ErrorPriority.High)
                          }
                          state {
                            do!
                              funJson
                              |> JsonValue.AsEnum(Set.singleton "Table")
                              |> state.OfSum
                              |> state.Map(ignore)

                            return!
                              state {
                                let! argsJson = (fields |> state.TryFindField "args")
                                let! arg = JsonValue.AsSingleton argsJson |> state.OfSum
                                let! arg = !arg
                                return ExprType.TableType arg
                              }
                              |> state.MapError(Errors.WithPriority ErrorPriority.High)
                          }
                          state {
                            do!
                              funJson
                              |> JsonValue.AsEnum(Set.singleton "Map")
                              |> state.OfSum
                              |> state.Map(ignore)

                            return!
                              state {
                                let! argsJson = (fields |> state.TryFindField "args")
                                let! key, value = JsonValue.AsPair argsJson |> state.OfSum
                                let! key, value = state.All2 !key !value
                                return ExprType.MapType(key, value)
                              }
                              |> state.MapError(Errors.WithPriority ErrorPriority.High)
                          }
                          state {
                            do!
                              funJson
                              |> JsonValue.AsEnum(Set.singleton "Sum")
                              |> state.OfSum
                              |> state.Map(ignore)

                            return!
                              state {
                                let! argsJson = (fields |> state.TryFindField "args")
                                let! leftJson, rightJson = JsonValue.AsPair argsJson |> state.OfSum
                                let! left, right = state.All2 !leftJson !rightJson
                                return ExprType.SumType(left, right)
                              }
                              |> state.MapError(Errors.WithPriority ErrorPriority.High)
                          }
                          state {
                            do!
                              funJson
                              |> JsonValue.AsEnum(Set.singleton "Union")
                              |> state.OfSum
                              |> state.Map(ignore)

                            return!
                              state {
                                let! argsJson = (fields |> state.TryFindField "args")
                                let! cases = argsJson |> JsonValue.AsArray |> state.OfSum

                                let! cases = state.All(cases |> Seq.map (ExprType.ParseUnionCase))

                                return
                                  ExprType.UnionType(
                                    cases |> Seq.map (fun c -> { CaseName = c.CaseName }, c) |> Map.ofSeq
                                  )
                              }
                              |> state.MapError(Errors.WithPriority ErrorPriority.High)
                          }
                          state {
                            do!
                              funJson
                              |> JsonValue.AsEnum(Set.singleton "KeyOf")
                              |> state.OfSum
                              |> state.Map(ignore)

                            return!
                              state {
                                let! argsJson = (fields |> state.TryFindField "args")
                                let! records = argsJson |> JsonValue.AsArray |> state.OfSum

                                let! records = state.All(records |> Seq.map (JsonValue.AsString >> state.OfSum))

                                if records.Length <> 1 then
                                  return!
                                    state.Throw(
                                      Errors.Singleton
                                        $"Error: cannot parse generic type {funJson}. Expected a single type name, found {records}"
                                    )
                                else
                                  let! record = records.[0] |> state.TryFindType
                                  let! record = record.Type |> ExprType.AsRecord |> state.OfSum

                                  return
                                    ExprType.UnionType(
                                      record
                                      |> Seq.map (fun c ->
                                        { CaseName = c.Key },
                                        { CaseName = c.Key
                                          Fields = ExprType.UnitType })
                                      |> Map.ofSeq
                                    )
                              }
                              |> state.MapError(Errors.WithPriority ErrorPriority.High)
                          }

                          state.Throw(Errors.Singleton $"Error: cannot parse generic type {funJson}")
                          |> state.MapError(Errors.WithPriority ErrorPriority.High) ]
                      )
                    )
                    |> state.MapError(Errors.HighestPriority)
                } ]
            )
          )
      }
      |> state.MapError(Errors.HighestPriority)
