module QueryBuilderComposer

type QueryPart = string
type Query = string
type QueryBuilder = string list
type QueryBuilderModifier = (QueryBuilder -> QueryBuilder)

// query builder functions
let add (queryBuilder: QueryBuilder) (part: QueryPart): QueryBuilder = 
    List.append queryBuilder [part]

let buildQuery (queryBuilder: QueryBuilder): Query = 
    queryBuilder 
    |> List.reduce (fun query part -> 
        if query.Length > 0 
            then query + "\n" + part 
            else part
        )

// query composer functions
let queryBuilderModifier (part: QueryPart) (queryBuilder: QueryBuilder): QueryBuilder =
    add queryBuilder part

let applyModifiers (modifiers: QueryBuilderModifier list) (queryBuilder: QueryBuilder): QueryBuilder =
    modifiers
    |> List.fold (fun queryBuilder modifier -> modifier queryBuilder) queryBuilder

let queryBuilderCompose (modifiers: QueryBuilderModifier list) (queryBuilder: QueryBuilder) =
    queryBuilder
    |> applyModifiers modifiers
    |> buildQuery

// define specific modifiers
let selectFooModifier = queryBuilderModifier "SELECT foo"
let fromBarModifier = queryBuilderModifier "FROM bar"
let approvedModifier = queryBuilderModifier "WHERE approved = true"

// build Query from QueryBuilder
let (queryBuilder: QueryBuilder) = []
let selectApprovedFooFromBar = queryBuilderCompose [selectFooModifier; fromBarModifier; approvedModifier] queryBuilder
