module rec Satisficer.Optimization

open Satisficer.Core

open FactoryFeatures



type FactoryFeatures<'T> =
    abstract member Init   : 'T
    abstract member Update : FactoryComponent -> 'T -> 'T


[<AutoOpen>]
module internal FactoryFeatures =

    type FactoryFeatures<'T> with

        member self.UpdateFlipped (t: 'T) (comp: FactoryComponent) : 'T =
            self.Update comp t

        member self.Evaluate (setup: FactorySetup) : 'T =
            List.fold self.UpdateFlipped self.Init setup.Components

        member self.UpdateMultiple (t: 'T) (comps: seq<FactoryComponent>) : 'T =
            Seq.fold self.UpdateFlipped t comps




type Order =
    | Lt
    | Eq
    | Gt
    | Unk

    static member inline ( + ) (o1, o2) =
        match o1, o2 with
        | Unk, _  | _, Unk -> Unk
        | Eq,  o  | o, Eq  -> o
        | _ when o1 = o2   -> o1
        | _                -> Unk

    static member inline ( * ) (o1, o2) =
        match o1, o2 with
        | Lt, _  | _, Lt   -> Lt
        | Eq, o  | o, Eq   -> o
        | _ when o1 = o2   -> o1
        | _                -> Unk


type Rating<'T, 'R when 'R : comparison> = 'T -> 'R[]

module Rating =

    let combine (rs: #seq<Rating<'T, 'R>>) : Rating<'T, 'R> =
        fun fs -> Array.concat [ for r in rs -> r fs ]

    let compare<'T when 'T : comparison> (rate1 : 'T[]) (rate2 : 'T[]) =
        assert (rate1.Length = rate2.Length)

        Seq.zip rate1 rate2
        |> Seq.fold
            (fun ord (r1, r2) ->
                let isLt = r1 < r2
                let isGt = r1 > r2
                match ord with
                | Lt when isGt -> Unk
                | Gt when isLt -> Unk
                | Eq when isLt -> Lt
                | Eq when isGt -> Eq
                | _ -> ord
            )
            Eq

    let producesExactAmount (item: Item) (amt: float</minute>) : Rating<#FactoryModification.HasResourceIO, float> =
        fun features ->
        [|
            match Map.tryFind item.Name features.Resources with
            | None        -> yield (- System.Math.Pow(float amt, 2.0))
            | Some (amt') -> yield (- System.Math.Pow(float amt - float amt', 2))
        |]



type Info<'T> =
    {
        ScoreVector : float[]
        Features    : 'T
    }


type Frontier<'T> =
    {
        Current  : Map<FactorySetup, Info<'T>>
        Features : FactoryFeatures<'T>
        Rating   : Rating<'T, float>
    }

    member self.Compose (mod1 : FactoryModification<'T>) (mod2 : FactoryModification<'T>) : FactoryModification<'T> =
        fun (setup, features) ->
            match mod1 (setup, features) with
            | None -> None
            | Some newComponents ->
            let setup' = { setup with Components = newComponents @ setup.Components }
            let features' = self.Features.UpdateMultiple features newComponents
            mod2 (setup', features')



module Frontier =

    let init (features: FactoryFeatures<'T>) (rating: Rating<'T, float>) (current: seq<FactorySetup>) : Frontier<'T> =
        {
            Features = features
            Rating   = rating
            Current  =
                Map [
                    for setup in current do
                        let feats = features.Evaluate(setup)
                        yield (setup, { Features = feats; ScoreVector = rating(feats) })
                ]
        }

    let internal pareFrontier (setups: struct(FactorySetup * Info<'T>) array) : Map<FactorySetup, Info<'T>> =
        setups
        |> Array.fold (fun (paredSetups: list<struct(FactorySetup * Info<'T>)>) newItem ->
            let struct(newSetup, newInfo) = newItem
            let overallOrd, filteredSetups =
                paredSetups
                |> List.fold (fun (order, paredSetups) item ->
                    let struct(_, info) = item
                    let order' = Rating.compare newInfo.ScoreVector info.ScoreVector
                    (
                        order * order',
                        match order' with
                        | Gt -> paredSetups
                        | _  -> item :: paredSetups
                    )
                ) (Eq, [])
            match overallOrd with
            | Gt | Eq -> [newItem]
            | Lt      -> filteredSetups
            | Unk     -> newItem :: filteredSetups
        ) []
        |> List.fold (fun m struct(newSetup, newInfo) ->
            Map.add newSetup newInfo m
        ) Map.empty


    let update (modf : FactoryModification<'T>) (frontier : Frontier<'T>) : Frontier<'T> =
        // new setups, which include old ones and additionally ones augmented by some modification.
        let allNewSetups =
            [|
                for KeyValue (setup, info) in frontier.Current do
                    yield struct(setup, info)
                    match modf (setup, info.Features) with
                    | None -> ()
                    | Some newComponents ->
                        let info' = Seq.fold frontier.Features.UpdateFlipped info.Features newComponents
                        yield struct({ Components = List.ofSeq newComponents @ setup.Components }, { ScoreVector = frontier.Rating(info'); Features = info' })
            |]

        // now we need to pare down this list to only those which are not inter-pareto-dominated.
        { frontier with Current = pareFrontier allNewSetups }




type FactoryModification<'T> = (FactorySetup * 'T) -> option<list<FactoryComponent>>

module FactoryModification =

    type HasResourceParts =
        abstract member Parts : ResourceIO.Parts

    let modifyWith (comp: FactoryComponent) : FactoryModification<'T> =
        fun _ -> Some [ comp ]

    let modifyOutputsChecked (comp: FactoryComponent) : FactoryModification<#HasResourceParts> =
        let componentParts = ResourceIO.parts comp.Resources
        fun (_, features) ->
            if ResourceIO.satisfies features.Parts.Produced componentParts.Consumed then
                Some [ comp ]
            else
                None

    let modifyInputsChecked (comp: FactoryComponent) : FactoryModification<#HasResourceParts> =
        let componentParts = ResourceIO.parts comp.Resources
        fun (_, features) ->
            match ResourceIO.scaleRequiredToSatisfy componentParts.Produced features.Parts.Consumed with
            | None -> None
            | Some scale -> Some [ comp.Scale(scale) ]

    type HasResourceIO =
        abstract member Resources : ResourceIO

    let scaleToFit (item: Item) (amt: float</minute>) : FactoryModification<#HasResourceIO> =
        fun (setup, features) ->
            match Map.tryFind item.Name features.Resources with
            | Some amt' when sign amt' = sign amt ->
                let scaleFactor = amt / amt' - 1.
                Some [
                    for comp in setup.Components ->
                        comp.Scale(scaleFactor)
                ]
            | _ -> None



type ModificationGenerator<'T> =
    abstract member NextMods : Frontier<'T> -> seq<FactoryModification<'T>>




type Optimizer<'T> (mods: ModificationGenerator<'T>) =

    member _.PerformRound (frontier: Frontier<'T>) : Frontier<'T> =
        mods.NextMods(frontier)
        |> Seq.fold (fun front modd ->
            Frontier.update modd front
        ) frontier