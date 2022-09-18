
open FSharp.Data
open FSharp.Data.Runtime.StructuralInference
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Microsoft.Extensions.FileProviders.Embedded

open Satisficer.Core
open Satisficer.Optimization
open Satisficer.Ingest

let embeddedProvider = EmbeddedResourceFileInfo(typeof<GameInfo>.Assembly, "Satisficer.Docs.json", "docs", System.DateTimeOffset.Now)
let info =
    embeddedProvider.CreateReadStream()
    |> GameInfo.Init

for recipe in info.Recipes.Values do
    printfn "%A" recipe


type FeatureCarrier =
    {
        Resources  : ResourceIO
        PartsCache : Lazy<ResourceIO.Parts>
    }

    interface FactoryModification.HasResourceIO with
        member self.Resources = self.Resources

    interface FactoryModification.HasResourceParts with
        member self.Parts = self.PartsCache.Force()


type Features =
    | Features

    interface FactoryFeatures<FeatureCarrier> with
        member _.Init =
            {
                Resources  = Map.empty
                PartsCache = Lazy.CreateFromValue { Produced = Map.empty; Consumed = Map.empty }
            }

        member _.Update comp feats =
            let resources' = ResourceIO.add feats.Resources comp.Resources
            { feats with
                Resources  = resources'
                PartsCache = lazy (ResourceIO.parts resources')
            }


type Mods =
    | Mods

    interface ModificationGenerator<FeatureCarrier> with
        member _.NextMods (_ : Frontier<FeatureCarrier>) =
            seq {
                for recipe in info.Recipes.Values ->
                    FactoryModification.modifyOutputsChecked (FactoryComponent.ofRecipe recipe 1.0)

                yield FactoryModification.scaleToFit info.Items["Steel Ingot"] 60.</minute>
            }

let optimizer = Optimizer(Mods)


let rate : Rating<FeatureCarrier, float> =
    Rating.combine [
        yield Rating.producesExactAmount info.Items["Steel Ingot"] 60.</minute>

        for item in info.Items.Values do
            if item.Name <> "Steel Ingot" then
                yield Rating.producesExactAmount item 0.</minute>
    ]



let mutable frontier =
    Frontier.init Features rate [
        {
            Components = [
                FactoryComponent.ofResource info.Items["Iron Ore"] 120.0</minute>
                FactoryComponent.ofResource info.Items["Coal"] 120.0</minute>
            ]
        }
    ]

for i in 0 .. 3 do
    frontier <- optimizer.PerformRound(frontier)
    printfn "Round %d : " i
    for KeyValue (setup, info) in frontier.Current do
        printfn "Setup : %A" setup
        printfn "Info  : %A" info