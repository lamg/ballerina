module Ballerina.Cat.Tests.BusinessRuleEngine.Next.Type.FormValidator

open System
open NUnit.Framework
open Ballerina.Collections.Sum
open Ballerina.Data.Schema.Model
open Ballerina.Data.Spec.Model
open Ballerina.DSL.Expr.Model
open Ballerina.DSL.Expr.Types.Model
open Ballerina.DSL.FormEngine.Model
open Ballerina.DSL.FormEngine.V2Validator
open Ballerina.DSL.Next.Types.Model

[<Test>]
let ``LangNext-Validate form compatibility against V2 types`` () =
  let formEntityName = "Customer"
  let entityTypeId = VarName.Create formEntityName

  let primitiveRenderer: PrimitiveRenderer =
    { PrimitiveRendererName = "TextInput"
      PrimitiveRendererId = Guid.Parse "00000000-0000-0000-0000-000000000001"
      Type = ExprType.PrimitiveType PrimitiveType.StringType
      Label = None }

  let fieldConfig: FieldConfig<unit, unit> =
    { FieldName = "name"
      FieldId = Guid.Parse "00000000-0000-0000-0000-000000000002"
      Label = None
      Tooltip = None
      Details = None
      Renderer = Renderer.PrimitiveRenderer primitiveRenderer
      Visible = Expr.Value Value.Unit
      Disabled = None }

  let formFields: FormFields<unit, unit> =
    { Fields = Map.ofList [ (fieldConfig.FieldName, fieldConfig) ]
      Disabled = FormGroup.Inlined []
      Tabs = { FormTabs = Map.empty } }

  let formBody: FormBody<unit, unit> =
    FormBody.Record
      {| Renderer = None
         Fields = formFields
         RecordType = ExprType.LookupType entityTypeId |}

  let formConfig: FormConfig<unit, unit> =
    { FormName = "CustomerForm"
      FormId = Guid.Parse "00000000-0000-0000-0000-000000000010"
      Body = formBody
      ContainerRenderer = None }

  let customerTypeExpr: TypeExpr =
    TypeExpr.Let(
      "name",
      TypeExpr.NewSymbol "name",
      TypeExpr.Record [ (TypeExpr.Lookup(Identifier.LocalScope "name"), TypeExpr.Primitive PrimitiveType.String) ]
    )

  let spec: Spec =
    { Name = SpecName "SimpleSpec"
      Body =
        { Schema =
            { Entities =
                Map.ofList [
                  ( formEntityName,
                    { Type = Identifier.LocalScope formEntityName |> TypeExpr.Lookup
                      Methods = Set.ofList [ EntityMethod.Get ]
                      Updaters = []
                      Predicates = Map.empty } )
                ]
              Lookups = Map.empty }
          TypesV2 = [ (formEntityName, customerTypeExpr) ] } }

  let entityApi = EntityApi.Create(formEntityName, entityTypeId)

  let ctx: ParsedFormsContext<unit, unit> =
    { ParsedFormsContext<unit, unit>.Empty with
        Apis =
          { FormApis<unit, unit>.Empty with
              Entities = Map.ofList [ (formEntityName, (entityApi, Set.empty<CrudMethod>)) ] } }

  let result =
    FormConfig<unit, unit>.Validate
      ctx
      formConfig
      spec

  match result with
  | Sum.Left () -> ()
  | Sum.Right errors -> Assert.Fail($"Expected validation to succeed but got errors: {errors}")
