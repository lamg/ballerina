namespace Ballerina.DSL.FormEngine.Codegen.Golang.LanguageConstructs


open Ballerina.DSL.FormEngine.Model
open Ballerina.Core
open Enum

type GolangStreamGETMANYers =
  { FunctionName: string
    StreamNotFoundErrorConstructor: string
    Tuple2Type: string
    TableType: string
    Ones:
      List<
        {| StreamName: string
           StreamLookupType: string
           StreamType: string |}
       > }

  static member Generate (ctx: GolangContext) (ones: GolangStreamGETMANYers) =
    StringBuilder.Many(
      seq {
        yield StringBuilder.One $"func {ones.FunctionName}[Id any, SearchParams any, Result any]("
        yield StringBuilder.One "\n"

        for t in ones.Ones do
          yield
            StringBuilder.Many(
              seq {
                yield
                  StringBuilder.One(
                    $"  get{t.StreamLookupType}__{t.StreamName} func (Id, SearchParams) ({ones.Tuple2Type}[{t.StreamLookupType}, {ones.TableType}[{t.StreamType}]],error), "
                  )

                yield StringBuilder.One "\n"

                yield
                  StringBuilder.One(
                    $"  serialize{t.StreamLookupType}__{t.StreamName} func ({ones.Tuple2Type}[{t.StreamLookupType}, {ones.TableType}[{t.StreamType}]]) (Result,error), "
                  )

                yield StringBuilder.One "\n"
              }
            )

        yield
          StringBuilder.One
            ") func (entityName string, apiName string, id Id, searchParams SearchParams) (Result,error) { return func (entityName string, apiName string, id Id, searchParams SearchParams) (Result, error) {\n"

        yield StringBuilder.One "    var resultNil Result;\n"
        yield StringBuilder.One "    switch (true) {\n"

        for t in ones.Ones do
          yield
            StringBuilder.One $$"""      case (apiName == "{{t.StreamName}}" && entityName == "{{t.StreamType}}"):  """

          yield StringBuilder.One "\n"

          yield
            StringBuilder.One
              $$"""        var res, err = get{{t.StreamLookupType}}__{{t.StreamName}}(id, searchParams);  """

          yield StringBuilder.One "\n"

          yield StringBuilder.One $$"""        if err != nil { return resultNil, err }  """
          yield StringBuilder.One "\n"
          yield StringBuilder.One $$"""        return serialize{{t.StreamLookupType}}__{{t.StreamName}}(res); """

          yield StringBuilder.One "\n"

        yield StringBuilder.One "    }\n"

        yield StringBuilder.One $"    return resultNil, {ones.StreamNotFoundErrorConstructor}(entityName, apiName);\n"

        yield StringBuilder.One "  }\n"
        yield StringBuilder.One "}\n\n"
      }
    )
