namespace Ballerina.DSL.FormEngine.Codegen.Golang.LanguageConstructs


open Ballerina.DSL.FormEngine.Model
open Ballerina.Core
open Enum

type GolangOnePATCHers =
  { FunctionName: string
    OneNotFoundErrorConstructor: string
    Tuple2Type: string
    DeltaBaseType: string
    DeltaOneType: string
    Ones:
      List<
        {| OneName: string
           OneLookupType: string
           OneType: string |}
       > }

  static member Generate (ctx: GolangContext) (ones: GolangOnePATCHers) =
    StringBuilder.Many(
      seq {
        yield StringBuilder.One $"func {ones.FunctionName}[Id any, Result any]("
        yield StringBuilder.One "\n"

        for t in ones.Ones do
          yield
            StringBuilder.Many(
              seq {
                yield
                  StringBuilder.One(
                    $"  deserialize{t.OneLookupType}__{t.OneName} func (Id, {ones.DeltaBaseType}) ({ones.Tuple2Type}[{t.OneLookupType}, {ones.DeltaOneType}[{t.OneType},Delta{t.OneType}]],error), "
                  )

                yield StringBuilder.One "\n"

                yield
                  StringBuilder.One(
                    $"  commit{t.OneLookupType}__{t.OneName} func ({ones.Tuple2Type}[{t.OneLookupType}, {ones.DeltaOneType}[{t.OneType},Delta{t.OneType}]]) (Result,error), "
                  )

                yield StringBuilder.One "\n"
              }
            )

        yield
          StringBuilder.One
            $") func (entityName string, apiName string, id Id, delta {ones.DeltaBaseType}) (Result,error) {{ return func (entityName string, apiName string, id Id, delta {ones.DeltaBaseType}) (Result, error) {{\n"

        yield StringBuilder.One "    var resultNil Result;\n"
        yield StringBuilder.One "    switch (true) {\n"

        for t in ones.Ones do
          yield StringBuilder.One $$"""      case (apiName == "{{t.OneName}}" && entityName == "{{t.OneType}}"):  """
          yield StringBuilder.One "\n"

          yield
            StringBuilder.One $$"""        var res, err = deserialize{{t.OneLookupType}}__{{t.OneName}}(id, delta);  """

          yield StringBuilder.One "\n"

          yield StringBuilder.One $$"""        if err != nil { return resultNil, err }  """
          yield StringBuilder.One "\n"
          yield StringBuilder.One $$"""        return commit{{t.OneLookupType}}__{{t.OneName}}(res); """

          yield StringBuilder.One "\n"

        yield StringBuilder.One "    }\n"

        yield StringBuilder.One $"    return resultNil, {ones.OneNotFoundErrorConstructor}(entityName, apiName);\n"

        yield StringBuilder.One "  }\n"
        yield StringBuilder.One "}\n\n"
      }
    )
