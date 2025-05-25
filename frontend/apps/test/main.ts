import {
  Sum,
  Option,
  Errors,
  ValueOrErrors,
  MapRepo,
  PredicateValueExtractor,
  PredicateValue,
  DispatchParsedType,
  Predicate,
  Renderer,
  BasicFun,
  Expr
} from "ballerina-core";
import { List, Map, Set } from "immutable";

export type EvalContext<T, Res> = {
  global: PredicateValue,
  root: PredicateValue,
  local: PredicateValue,
  traversalIterator: PredicateValue,
}

export type TraversalContext<T, Res> = {
  types: Map<string, DispatchParsedType<T>>;
  forms: Map<string, Renderer<T>>;
  joinRes: BasicFun<[Res, Res], Res>
  traverseSingleType: Traversal<T, Res>
}

export type Traversal<T, Res> =
  BasicFun<
    DispatchParsedType<T>,
    Option< // based on the type provided, it can return `None` depending on some predicate such as "there are no children with the desired type T that we are searching". This is an important performance optimization
      BasicFun<
        EvalContext<T, Res>, // the proper dynamic part of the evaluation depends solely on the eval context (root, global, local) and the actual value being traversed
        ValueOrErrors<Res, string>
      >
    >
  >

export type ValueTraversal<T, Res> =
  BasicFun<
    EvalContext<T, Res>, // the proper dynamic part of the evaluation depends solely on the eval context (root, global, local) and the actual value being traversed
    ValueOrErrors<Res, string>
  >

export const RendererTraversal = {
  Operations: {
    Run: <T, Res>(
      type: DispatchParsedType<T>,
      renderer: Renderer<T>,
      traversalContext: TraversalContext<T, Res>
    ): ValueOrErrors<Option<ValueTraversal<T, Res>>, string> => {
      const rec = RendererTraversal.Operations.Run<T, Res>
      const allTraversal = ValueOrErrors.Operations.All<Option<ValueTraversal<T, Res>>, string>
      const retTraversal = ValueOrErrors.Default.return<Option<ValueTraversal<T, Res>>, string>
      const throwOneTraversal = ValueOrErrors.Default.throwOne<Option<ValueTraversal<T, Res>>, string>
      const allEval = ValueOrErrors.Operations.All<Res, string>
      const retEval = ValueOrErrors.Default.return<Res, string>
      const throwOneEval = ValueOrErrors.Default.throwOne<Res, string>

      const traverseNode = traversalContext.traverseSingleType(type)
      if (type.kind == "record" && renderer.kind == "recordRenderer") {
        return ValueOrErrors.Operations.All(
          List(
            type.fields.map((fieldType, fieldName) =>
              MapRepo.Operations.tryFindWithError(fieldName, renderer.fields, () => `Error: cannot find field ${fieldName} in renderer for type ${type.name}`)
                .Then(fieldRenderer =>
                  rec(fieldType, fieldRenderer.renderer, traversalContext)
                  .Then(fieldTraversal => {
                    return ValueOrErrors.Default.return({ fieldName:fieldName, visibility:fieldRenderer.visible, fieldTraversal:fieldTraversal})
                  })
                )
            ).valueSeq()
          )
        ).Then(fieldTraversals => {
          if (fieldTraversals.every(f => f.fieldTraversal.kind == "l") && traverseNode.kind == "l") {
            return retTraversal(Option.Default.none())
          }
          return retTraversal(Option.Default.some((evalContext: EvalContext<T, Res>) =>{
            if (!PredicateValue.Operations.IsRecord(evalContext.traversalIterator))
              return throwOneEval(`Error: traversal iterator is not a record, got ${evalContext.traversalIterator}`)
            const traversalIteratorFields = evalContext.traversalIterator.fields
            return allEval(fieldTraversals.flatMap(f => {
                if (f.fieldTraversal.kind == "l") return [] 
                if (f.visibility != undefined) {
                  const visible = Expr.Operations.EvaluateAsBoolean(Map([["global", evalContext.global], ["local", evalContext.local], ["root", evalContext.root]]))(f.visibility)
                  if (visible.kind == "value" && !visible.value) {
                    return []
                  }
                }
                return [f.fieldTraversal.value({ ...evalContext, traversalIterator: traversalIteratorFields.get(f.fieldName)! })]
              }))
                .Then((fieldResults: List<Res>) =>
                  (traverseNode.kind == "r") ?
                    traverseNode.value(evalContext).Then((nodeResult: Res) =>
                      retEval(fieldResults.reduce((acc, res) => traversalContext.joinRes([acc, res]), nodeResult))
                    )
                    :
                    retEval(fieldResults.reduce((acc, res) => traversalContext.joinRes([acc, res])))
                )
              }))
        })
      }
      return null!
    }
  }
}
