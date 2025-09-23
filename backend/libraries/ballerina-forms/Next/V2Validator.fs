namespace Ballerina.DSL.FormEngine

module V2Validator =
  open Ballerina.DSL.FormEngine.Model
  open Ballerina.Collections.Sum
  open Ballerina.Errors
  
  type FormConfig<'ExprExtension, 'ValueExtension> with
    static member Validate
      (_config: CodeGenConfig)
      (_ctx: ParsedFormsContext<'ExprExtension, 'ValueExtension>)
      (_formConfig: FormConfig<'ExprExtension, 'ValueExtension>): Sum<Unit, Errors> =
      failwith "not implemented"