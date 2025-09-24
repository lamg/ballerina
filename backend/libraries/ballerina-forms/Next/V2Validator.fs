namespace Ballerina.DSL.FormEngine

// Approach:
// - get the V1 type from the FormConfig
// - translate the V1 type to V2
// - evaluate and unify the V2 types
// - collect the errors from the above step if any
//
// In the end absense of errors means the validation was successful

module V2Validator =
  open Ballerina.DSL.FormEngine.Model
  open Ballerina.DSL.Next.Types
  open Ballerina.DSL.Next.Types.Eval
  open Ballerina.DSL.Next.Unification
  open Ballerina.Collections.Sum
  open Ballerina.Errors
  open Ballerina.Data.Spec.Model
  open Ballerina.Data.Spec.Builder
  open Ballerina.State.WithError
  open Ballerina.Data.TypeEval
  open Ballerina.Data
  open Ballerina.DSL.Expr.Model

  let private identifier name = Identifier.LocalScope name

  let private wrapWithSymbolLets (names: List<string>) (body: TypeExpr) =
    let xs = List.distinct names
    body
    |> List.foldBack (fun name acc -> TypeExpr.Let(name, TypeExpr.NewSymbol name, acc)) xs

  let rec private kindOf (kind: ExprTypeKind) : Kind =
    match kind with
    | ExprTypeKind.Star -> Kind.Star
    | ExprTypeKind.Arrow(input, output) -> Kind.Arrow(kindOf input, kindOf output)

  let rec v2ofV1 (t: ExprType) =
    let applyGeneric typeName args =
      args
      |> List.fold (fun acc arg -> TypeExpr.Apply(acc, arg)) (TypeExpr.Lookup(identifier typeName))

    match t with
    | ExprType.UnitType -> TypeExpr.Primitive PrimitiveType.Unit
    | ExprType.CustomType name -> TypeExpr.Lookup(identifier name)
    | ExprType.VarType v -> TypeExpr.Lookup(identifier v.VarName)
    | ExprType.LookupType id -> TypeExpr.Lookup(identifier id.VarName)
    | ExprType.KeyOf(inner, excluded) ->
      let baseKeyType = v2ofV1 inner |> TypeExpr.KeyOf

      match excluded with
      | [] -> baseKeyType
      | _ ->
        let exclusions =
          excluded
          |> List.map (fun name -> TypeExpr.Lookup(identifier name), TypeExpr.Primitive PrimitiveType.Unit)
          |> TypeExpr.Record
          |> wrapWithSymbolLets excluded

        TypeExpr.Exclude(baseKeyType, exclusions)
    | ExprType.PrimitiveType primitive ->
      match primitive with
      | PrimitiveType.BoolType -> TypeExpr.Primitive PrimitiveType.Bool
      | PrimitiveType.DateOnlyType -> TypeExpr.Primitive PrimitiveType.DateOnly
      | PrimitiveType.DateTimeType -> TypeExpr.Primitive PrimitiveType.DateTime
      | PrimitiveType.FloatType -> TypeExpr.Primitive PrimitiveType.Float64
      | PrimitiveType.GuidType
      | PrimitiveType.EntityIdUUIDType -> TypeExpr.Primitive PrimitiveType.Guid
      | PrimitiveType.IntType -> TypeExpr.Primitive PrimitiveType.Int32
      | PrimitiveType.StringType
      | PrimitiveType.EntityIdStringType
      | PrimitiveType.CalculatedDisplayValueType -> TypeExpr.Primitive PrimitiveType.String
    | ExprType.RecordType fields ->
      let items =
        fields
        |> Map.toList
        |> List.map (fun (name, fieldType) -> name, v2ofV1 fieldType)

      items
      |> List.map (fun (name, fieldType) -> TypeExpr.Lookup(identifier name), fieldType)
      |> TypeExpr.Record
      |> wrapWithSymbolLets (items |> List.map fst)
    | ExprType.UnionType cases ->
      let items =
        cases
        |> Map.toList
        |> List.map (fun (caseName, unionCase) -> caseName.CaseName, v2ofV1 unionCase.Fields)

      items
      |> List.map (fun (name, caseType) -> TypeExpr.Lookup(identifier name), caseType)
      |> TypeExpr.Union
      |> wrapWithSymbolLets (items |> List.map fst)
    | ExprType.MapType(keyType, valueType) -> TypeExpr.Map(v2ofV1 keyType, v2ofV1 valueType)
    | ExprType.SumType(left, right) -> TypeExpr.Sum [ v2ofV1 left; v2ofV1 right ]
    | ExprType.TupleType elements -> elements |> List.map v2ofV1 |> TypeExpr.Tuple
    | ExprType.OptionType inner -> applyGeneric "Option" [ v2ofV1 inner ]
    | ExprType.OneType inner -> applyGeneric "One" [ v2ofV1 inner ]
    | ExprType.ReadOnlyType inner -> applyGeneric "ReadOnly" [ v2ofV1 inner ]
    | ExprType.ManyType inner -> applyGeneric "Many" [ v2ofV1 inner ]
    | ExprType.ListType inner -> applyGeneric "List" [ v2ofV1 inner ]
    | ExprType.TableType inner -> applyGeneric "Table" [ v2ofV1 inner ]
    | ExprType.SetType inner -> TypeExpr.Set(v2ofV1 inner)
    | ExprType.ArrowType(input, output) -> TypeExpr.Arrow(v2ofV1 input, v2ofV1 output)
    | ExprType.GenericType(varId, kind, body) ->
      TypeExpr.Lambda(
        { Name = varId.VarName
          Kind = kindOf kind },
        v2ofV1 body
      )
    | ExprType.GenericApplicationType(generic, argument) -> TypeExpr.Apply(v2ofV1 generic, v2ofV1 argument)
    | ExprType.TranslationOverride label ->
      failwith $"TranslationOverride {label} is not supported in V2 translation."

  type FormConfig<'ExprExtension, 'ValueExtension> with
    static member Validate
      (_config: CodeGenConfig)
      (ctx: ParsedFormsContext<'ExprExtension, 'ValueExtension>)
      (formConfig: FormConfig<'ExprExtension, 'ValueExtension>)
      (spec: Spec)
      : Sum<Unit, Errors> =
      state {
        do! typeContextFromSpecBody spec.Body
        let! v1Translated, _ = formConfig.Body |> FormBody.FormDeclarationType |> v2ofV1 |> TypeExpr.Eval
        let! v2Schema = Schema.Model.Schema.SchemaEval spec.Body.Schema
        let! env = state.GetState()

        match FormBody.FormDeclarationType formConfig.Body with
        | ExprType.LookupType id ->
          let entity, _ = ctx.Apis.Entities[id.VarName]

          let v2Type = v2Schema.Entities[entity.EntityName].Type

          let unifyResult =
            TypeValue.Unify(v1Translated, v2Type) |> State.Run(env, UnificationState.Empty)

          match unifyResult with
          | Sum.Left _ -> ()
          | Sum.Right(errors, _) -> do! state.Throw errors
        | _ -> ()
      }
      |> State.Run(Eval.TypeExprEvalContext.Empty, Eval.TypeExprEvalState.Empty)
      |> function
        | Sum.Left(_, _) -> Sum.Left()
        | Sum.Right(errors, _) -> Sum.Right errors
