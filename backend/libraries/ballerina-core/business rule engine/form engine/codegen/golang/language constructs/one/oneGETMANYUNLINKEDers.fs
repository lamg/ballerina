namespace Ballerina.DSL.FormEngine.Codegen.Golang.LanguageConstructs


open Ballerina.DSL.FormEngine.Model
open Ballerina.Core
open Enum

type GolangOneGETMANYUNLINKEDers =
  { FunctionName: string
    OneNotFoundErrorConstructor: string
    Tuple2Type: string
    ManyType: string
    Ones:
      List<
        {| OneName: string
           OneLookupType: string
           OneType: string |}
       > }

  static member Generate (ctx: GolangContext) (ones: GolangOneGETMANYUNLINKEDers) =
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
                    $"  get{t.OneLookupType}__{t.OneName} func (Id, SearchParams) ({ones.Tuple2Type}[{t.OneLookupType}, {ones.ManyType}[{t.OneType}]],error), "
                  )

                yield StringBuilder.One "\n"

                yield
                  StringBuilder.One(
                    $"  serialize{t.OneLookupType}__{t.OneName} func ({ones.Tuple2Type}[{t.OneLookupType}, {ones.ManyType}[{t.OneType}]]) (Result,error), "
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
          yield StringBuilder.One $$"""      case (apiName == "{{t.OneName}}" && entityName == "{{t.OneType}}"):  """
          yield StringBuilder.One "\n"

          yield
            StringBuilder.One $$"""        var res, err = get{{t.OneLookupType}}__{{t.OneName}}(id, searchParams);  """

          yield StringBuilder.One "\n"

          yield StringBuilder.One $$"""        if err != nil { return resultNil, err }  """
          yield StringBuilder.One "\n"
          yield StringBuilder.One $$"""        return serialize{{t.OneLookupType}}__{{t.OneName}}(res); """

          yield StringBuilder.One "\n"

        yield StringBuilder.One "    }\n"

        yield StringBuilder.One $"    return resultNil, {ones.OneNotFoundErrorConstructor}(entityName, apiName);\n"

        yield StringBuilder.One "  }\n"
        yield StringBuilder.One "}\n\n"
      }
    )
