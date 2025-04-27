namespace Ballerina.DSL.FormEngine.Codegen.Golang.LanguageConstructs


open Ballerina.DSL.FormEngine.Model
open Ballerina.Core
open Enum

type GolangManyPATCHers =
  { FunctionName: string
    ManyNotFoundErrorConstructor: string
    Tuple2Type: string
    DeltaBaseType: string
    DeltaManyType: string
    Manys:
      List<
        {| ManyName: string
           ManyLookupType: string
           ManyType: string |}
       > }

  static member Generate (ctx: GolangContext) (manys: GolangManyPATCHers) =
    StringBuilder.Many(
      seq {
        yield StringBuilder.One $"func {manys.FunctionName}[Id any, Result any]("
        yield StringBuilder.One "\n"

        for t in manys.Manys do
          yield
            StringBuilder.Many(
              seq {
                yield
                  StringBuilder.One(
                    $"  deserialize{t.ManyLookupType}__{t.ManyName} func (Id, {manys.DeltaBaseType}) ({manys.Tuple2Type}[{t.ManyLookupType}, {manys.DeltaManyType}[{t.ManyType},Delta{t.ManyType}]],error), "
                  )

                yield StringBuilder.One "\n"

                yield
                  StringBuilder.One(
                    $"  commit{t.ManyLookupType}__{t.ManyName} func ({manys.Tuple2Type}[{t.ManyLookupType}, {manys.DeltaManyType}[{t.ManyType},Delta{t.ManyType}]]) (Result,error), "
                  )

                yield StringBuilder.One "\n"
              }
            )

        yield
          StringBuilder.One
            $") func (entityName string, apiName string, id Id, delta {manys.DeltaBaseType}) (Result,error) {{ return func (entityName string, apiName string, id Id, delta {manys.DeltaBaseType}) (Result, error) {{\n"

        yield StringBuilder.One "    var resultNil Result;\n"
        yield StringBuilder.One "    switch (true) {\n"

        for t in manys.Manys do
          yield StringBuilder.One $$"""      case (apiName == "{{t.ManyName}}" && entityName == "{{t.ManyType}}"):  """
          yield StringBuilder.One "\n"

          yield
            StringBuilder.One
              $$"""        var res, err = deserialize{{t.ManyLookupType}}__{{t.ManyName}}(id, delta);  """

          yield StringBuilder.One "\n"

          yield StringBuilder.One $$"""        if err != nil { return resultNil, err }  """
          yield StringBuilder.One "\n"
          yield StringBuilder.One $$"""        return commit{{t.ManyLookupType}}__{{t.ManyName}}(res); """

          yield StringBuilder.One "\n"

        yield StringBuilder.One "    }\n"

        yield StringBuilder.One $"    return resultNil, {manys.ManyNotFoundErrorConstructor}(entityName, apiName);\n"

        yield StringBuilder.One "  }\n"
        yield StringBuilder.One "}\n\n"
      }
    )
