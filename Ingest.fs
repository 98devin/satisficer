module Satisficer.Ingest

open FSharp.Data
open FSharp.Data.Runtime.StructuralInference
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

open Satisficer.Core

[<Literal>]
let itemDescriptorSample = """
{
    "ClassName": "Desc_NuclearWaste_C",
    "mDisplayName": "Uranium Waste",
    "mDescription": "The by-product of consuming Uranium Fuel Rods in the Nuclear Power Plant.\r\nNon-fissible Uranium can be extracted. Handle with caution.\r\n\r\nCaution: HIGHLY Radioactive.",
    "mAbbreviatedDisplayName": "",
    "mStackSize": "SS_HUGE",
    "mCanBeDiscarded": "False",
    "mRememberPickUp": "False",
    "mEnergyValue": "0.000000",
    "mRadioactiveDecay": "10.000000",
    "mForm": "RF_SOLID",
    "mSmallIcon": "Texture2D /Game/FactoryGame/Resource/Parts/NuclearWaste/UI/IconDesc_NuclearWaste_64.IconDesc_NuclearWaste_64",
    "mPersistentBigIcon": "Texture2D /Game/FactoryGame/Resource/Parts/NuclearWaste/UI/IconDesc_NuclearWaste_256.IconDesc_NuclearWaste_256",
    "mSubCategories": "",
    "mMenuPriority": "0.000000",
    "mFluidColor": "(B=0,G=0,R=0,A=0)",
    "mGasColor": "(B=0,G=0,R=0,A=0)",
    "mBuildMenuPriority": "0.000000"
}
"""

[<Literal>]
let recipeDescriptorSample = """
{
    "ClassName": "Recipe_AILimiter_C",
    "FullName": "BlueprintGeneratedClass /Game/FactoryGame/Recipes/Assembler/Recipe_AILimiter.Recipe_AILimiter_C",
    "mDisplayName": "AI Limiter",
    "mIngredients": "((ItemClass=BlueprintGeneratedClass'\"/Game/FactoryGame/Resource/Parts/CopperSheet/Desc_CopperSheet.Desc_CopperSheet_C\"',Amount=5),(ItemClass=BlueprintGeneratedClass'\"/Game/FactoryGame/Resource/Parts/HighSpeedWire/Desc_HighSpeedWire.Desc_HighSpeedWire_C\"',Amount=20))",
    "mProduct": "((ItemClass=BlueprintGeneratedClass'\"/Game/FactoryGame/Resource/Parts/CircuitBoardHighSpeed/Desc_CircuitBoardHighSpeed.Desc_CircuitBoardHighSpeed_C\"',Amount=1))",
    "mManufacturingMenuPriority": "4.000000",
    "mManufactoringDuration": "12.000000",
    "mManualManufacturingMultiplier": "1.000000",
    "mProducedIn": "(/Game/FactoryGame/Buildable/Factory/AssemblerMk1/Build_AssemblerMk1.Build_AssemblerMk1_C,/Game/FactoryGame/Buildable/-Shared/WorkBench/BP_WorkBenchComponent.BP_WorkBenchComponent_C,/Script/FactoryGame.FGBuildableAutomatedWorkBench)",
    "mRelevantEvents": "",
    "mVariablePowerConsumptionConstant": "0.000000",
    "mVariablePowerConsumptionFactor": "1.000000"
}
"""

[<Literal>]
let manufacturerDescriptorSample = """
{
    "ClassName": "Build_ConstructorMk1_C",
    "IsPowered": "False",
    "mCurrentRecipeCheck": "",
    "mPreviousRecipeCheck": "",
    "CurrentPotentialConvert": "((1, 1.000000),(2, 1.200000),(0, 0.650000))",
    "mCurrentRecipeChanged": "()",
    "mManufacturingSpeed": "1.000000",
    "mFactoryInputConnections": "",
    "mPipeInputConnections": "",
    "mFactoryOutputConnections": "",
    "mPipeOutputConnections": "",
    "mPowerConsumption": "typeof<float>",
    "mPowerConsumptionExponent": "1.600000",
    "mDoesHaveShutdownAnimation": "False",
    "mOnHasPowerChanged": "()",
    "mOnHasProductionChanged": "()",
    "mOnHasStandbyChanged": "()",
    "mMinimumProducingTime": "0.000000",
    "mMinimumStoppedTime": "0.000000",
    "mNumCyclesForProductivity": "20",
    "mCanChangePotential": "True",
    "mMinPotential": "0.010000",
    "mMaxPotential": "1.000000",
    "mMaxPotentialIncreasePerCrystal": "0.500000",
    "mFluidStackSizeDefault": "SS_FLUID",
    "mFluidStackSizeMultiplier": "1",
    "OnReplicationDetailActorCreatedEvent": "()",
    "mEffectUpdateInterval": "0.000000",
    "mCachedSkeletalMeshes": "",
    "mAddToSignificanceManager": "True",
    "mSignificanceRange": "8000.000000",
    "mDisplayName": "Constructor",
    "mDescription": "Crafts one part into another part.\r\n\r\nCan be automated by feeding parts into it with a conveyor belt connected to the input. The produced parts can be automatically extracted by connecting a conveyor belt to the output.",
    "MaxRenderDistance": "-1.000000",
    "mHighlightVector": "(X=0.000000,Y=0.000000,Z=0.000000)",
    "mAllowColoring": "True",
    "mSkipBuildEffect": "False",
    "mBuildEffectSpeed": "0.000000",
    "mForceNetUpdateOnRegisterPlayer": "False",
    "mToggleDormancyOnInteraction": "False",
    "mShouldShowHighlight": "False",
    "mShouldShowAttachmentPointVisuals": "False",
    "mCreateClearanceMeshRepresentation": "True",
    "mAttachmentPoints": "",
    "mInteractingPlayers": "",
    "mIsUseable": "True",
    "mHideOnBuildEffectStart": "False",
    "mShouldModifyWorldGrid": "True"
}
"""

type ItemDescriptors = JsonProvider<itemDescriptorSample, InferenceMode=InferenceMode.ValuesAndInlineSchemasHints>
type ItemDescriptor = ItemDescriptors.Root

type RecipeDescriptors = JsonProvider<recipeDescriptorSample, InferenceMode=InferenceMode.ValuesAndInlineSchemasHints>
type RecipeDescriptor = RecipeDescriptors.Root

type ManufacturerDescriptors = JsonProvider<manufacturerDescriptorSample, InferenceMode=InferenceMode.ValuesAndInlineSchemasHints>
type ManufacturerDescriptor = ManufacturerDescriptors.Root


module internal Utility =

    // referenced from: https://satisfactory.fandom.com/wiki/Module:DecodeHelper
    let stackEnumToSize s =
        match s with
        | "SS_ONE"    -> 1.</stack>
        | "SS_SMALL"  -> 50.</stack>
        | "SS_MEDIUM" -> 100.</stack>
        | "SS_BIG"    -> 200.</stack>
        | "SS_HUGE"   -> 500.</stack>
        | "SS_FLUID"  -> 0.</stack>
        | _           -> failwith "Value should not be in Data.json!"


    let producers = Regex(@"\((?:,?[^.]*\.(?<producer>[^,]+))*\)", RegexOptions.Compiled)
    let parseProducedIn s : seq<string> =
        let m = producers.Match(s)
        let caps = m.Groups["producer"].Captures
        [ for cap in caps -> cap.ToString() ]


    let ingredients = Regex(@"\((?:,?\(ItemClass=[^.]*\.(?<item>[^""]*)[^,]*,Amount=(?<amount>\d+)\))*\)", RegexOptions.Compiled)
    let parseIngredients s : seq<string * float</minute>> =
        let m = ingredients.Match(s)
        let caps = Seq.zip (m.Groups["item"].Captures) (m.Groups["amount"].Captures)
        [
            for (item, amount) in caps ->
                (
                    item.ToString(),
                    float (amount.ToString()) * 1.</minute>
                )
        ]


type GameInfo with

    static member Init (docs: Stream) : GameInfo =

        let items           = Dictionary()
        let machines        = Dictionary()
        let recipes         = Dictionary()
        let recipesByInput  = Dictionary()
        let recipesByOutput = Dictionary()

        let addToList (v : 'v) (d : Dictionary<'k, List<'v>>) (k : 'k) =
            if not (d.ContainsKey(k)) then
                d.Add(k, List())
            d[k].Add(v)

        let docsJson = JsonValue.Load(docs)
        let allClasses = Dictionary<string, JsonValue[]>()
        for classList in docsJson.AsArray() do
            allClasses.Add(classList["NativeClass"].AsString(), classList["Classes"].AsArray())

        let itemsRaw = Dictionary<string, ItemDescriptor>()
        let recipesRaw = Dictionary<string, RecipeDescriptor>()
        let manufacturersRaw = Dictionary<string, ManufacturerDescriptor>()

        let itemsClasses = [
            "Class'/Script/FactoryGame.FGBuildingDescriptor'"
            "Class'/Script/FactoryGame.FGConsumableDescriptor'"
            "Class'/Script/FactoryGame.FGEquipmentDescriptor'"
            "Class'/Script/FactoryGame.FGItemDescAmmoTypeColorCartridge'"
            "Class'/Script/FactoryGame.FGItemDescAmmoTypeInstantHit'"
            "Class'/Script/FactoryGame.FGItemDescAmmoTypeProjectile'"
            "Class'/Script/FactoryGame.FGItemDescriptor'"
            "Class'/Script/FactoryGame.FGItemDescriptorBiomass'"
            "Class'/Script/FactoryGame.FGItemDescriptorNuclearFuel'"
            "Class'/Script/FactoryGame.FGResourceDescriptor'"
        ]

        for itemClassName in itemsClasses do
            for classDesc in allClasses[itemClassName] do
                let itemDesc = ItemDescriptors.Load(classDesc)
                if itemDesc.MDisplayName <> "" then
                    itemsRaw.Add(itemDesc.ClassName, itemDesc)

        for classDesc in allClasses["Class'/Script/FactoryGame.FGRecipe'"] do
            let recipeDesc = RecipeDescriptors.Load(classDesc)
            recipesRaw.Add(recipeDesc.ClassName, recipeDesc) |> ignore

        for classDesc in allClasses["Class'/Script/FactoryGame.FGBuildableManufacturer'"] do
            let manufacturerDesc = ManufacturerDescriptors.Load(classDesc)
            manufacturersRaw.Add(manufacturerDesc.ClassName, manufacturerDesc)

        for itemRaw in itemsRaw.Values do
            items.Add(itemRaw.MDisplayName, {
                Name       = itemRaw.MDisplayName
                StackSize  = Utility.stackEnumToSize itemRaw.MStackSize
                SinkPoints =
                    itemRaw.JsonValue.TryGetProperty("mResourceSinkPoints")
                    |> Option.map (fun value -> float (value.AsString()))
            })

        for manufacturerRaw in manufacturersRaw.Values do
            machines.Add(manufacturerRaw.MDisplayName, {
                Name  = manufacturerRaw.MDisplayName
                Power = manufacturerRaw.MPowerConsumption * 1.<MW>
            })

        for recipeRaw in recipesRaw.Values do
            let producers = Utility.parseProducedIn recipeRaw.MProducedIn

            let inputs = Map [
                for itemRawName, amt in Utility.parseIngredients recipeRaw.MIngredients do
                    match itemsRaw.TryGetValue(itemRawName) with
                    | (true, itemRaw) -> yield (itemRaw.MDisplayName, amt)
                    | _               -> ()
            ]

            let outputs = Map [
                for itemRawName, amt in Utility.parseIngredients recipeRaw.MProduct do
                    match itemsRaw.TryGetValue(itemRawName) with
                    | (true, itemRaw) -> yield (itemRaw.MDisplayName, amt)
                    | _               -> ()
            ]

            for producer in producers do
            match manufacturersRaw.TryGetValue(producer) with
            | (false, _)    -> ()
            | (true, manufacturerRaw) ->
                let recipe =
                    {
                        Name       = recipeRaw.MDisplayName
                        Machine    = machines[manufacturerRaw.MDisplayName]
                        Resources = ResourceIO.sub outputs inputs
                    }
                recipes.Add(recipeRaw.MDisplayName, recipe)

                for input in inputs.Keys do
                    addToList recipe recipesByInput input
                for output in outputs.Keys do
                    addToList recipe recipesByOutput output

        let inline dictToMap d =
            d |> Seq.map (fun (KeyValue p) -> p) |> Map

        let inline dictToMapBy f d =
            d |> Seq.map (fun (KeyValue (k, v)) -> (k, f v)) |> Map

        {
            Items    = dictToMap items
            Machines = dictToMap machines
            Recipes  = dictToMap recipes

            RecipesByInput  = dictToMapBy Array.ofSeq recipesByInput
            RecipesByOutput = dictToMapBy Array.ofSeq recipesByOutput
        }

