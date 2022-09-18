module Satisficer.Core

[<Measure>]
type minute

[<Measure>]
type MW

[<Measure>]
type stack

type Item =
    {
        Name       : string
        StackSize  : float</stack>
        SinkPoints : option<float>
    }

type Machine =
    {
        Name  : string
        Power : float<MW>
    }

module Machine =

    let overclockedPower clockPercent machine =
        machine.Power * float (System.Math.Pow(float clockPercent / 100.0, 1.6))


type ResourceIO = Map<string, float</minute>>


module ResourceIO =
    type Parts =
        {
            Produced : ResourceIO // all positive components
            Consumed : ResourceIO // all negative components
        }

    let scale s (rio: ResourceIO) =
        rio
        |> Map.map (fun _ v  -> v * s)

    let merge f (rio1: ResourceIO) (rio2: ResourceIO) =
        (rio1, rio2) ||> Map.fold (fun m k v' ->
            m
            |> Map.change k
                (function
                | Some v -> Some (f v           v')
                | None   -> Some (f 0.</minute> v')
                )
        )

    let add = merge (+)
    let sub = merge (-)

    let parts (r: ResourceIO) =
        let (p, c) = Map.partition (fun _ v -> v > 0.</minute>) r
        { Produced = p; Consumed = c }

    let satisfies (r1: ResourceIO) (r2: ResourceIO) =
        Map.forall (fun rsc amt ->
            match Map.tryFind rsc r1 with
            | Some amt' -> (amt' + amt) > 0.</minute>
            | None      -> amt          > 0.</minute>
        ) r2

    let scaleRequiredToSatisfy (r1: ResourceIO) (r2: ResourceIO) : option<float> =
        r2 |>
        Map.fold (fun scale rsc amt ->
            match scale with
            | None        -> None
            | Some(scale) ->
            match Map.tryFind rsc r1 with
            | None        -> None
            | Some(amt')  ->
            let requiredScale = (amt' / amt) * -1.
            Some (max scale requiredScale)
        ) (Some 0.)



type Recipe =
    {
        Name      : string
        Machine   : Machine
        Resources : ResourceIO
    }


type GameInfo =
    {
        Items    : Map<string, Item>
        Machines : Map<string, Machine>
        Recipes  : Map<string, Recipe>

        RecipesByInput  : Map<string, Recipe[]>
        RecipesByOutput : Map<string, Recipe[]>
    }


type FactoryComponent =
    | Recipe   of Recipe : Recipe * Count  : float // fractional if there is one underclocked for example
    | Resource of Item   : Item   * Amount : float</minute>

    member self.Resources =
        match self with
        | Recipe (r, count) -> ResourceIO.scale count r.Resources
        | Resource (i, amt) -> Map [ (i.Name, amt) ]

    member self.Scale (by : float) : FactoryComponent =
        match self with
        | Recipe (r, count) -> Recipe (r, count * by)
        | Resource (i, amt) -> Resource (i, amt * by)


module FactoryComponent =

    let ofRecipe (r : Recipe) (scale : float) : FactoryComponent =
        Recipe (r, scale)

    let ofResource (i : Item) (amount : float</minute>) : FactoryComponent =
        Resource (i, amount)


type FactorySetup =
    {
        Components : list<FactoryComponent>
    }

module FactorySetup =

    let empty: FactorySetup =
        {
            Components = []
        }

    let ofRecipe (r : Recipe) (scale : float) : FactorySetup =
        {
            Components =
                [
                    FactoryComponent.ofRecipe r scale
                ]
        }