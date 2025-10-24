# dimension
A simple library oriented around providing easy and usable string-based units while still remaining extensible.

## Example

    worldPopulationInBillions = dimension "billion*people" 8.142
    worldPopulation = fmap floor $ applypos "billion" (*1e9) worldPopulationInBillions
    daysInYear = dimension "days/years" 365
    caloriePerDay = dimension "calories/days/people" 2000    
    caloriesPerYear = caloriePerDay !* daysInYear !* worldPopulation 
    > 5943659999270000 calories / years
Since everything has units, I chose `Symbols` to be the core dimensional type of this project.

## Implementation
A dimension is difined quite simply as: 

    type Dimension :: forall k. [(k,Int')] -> Type -> Type 
    newtype Dimension a b = MkDimension b
        deriving stock (Eq,Ord,Functor)

`k` represents the kind used to index dimensions. One example is `Symbols`, another would be a `Meter` kind, another might be `CaseInsenstiveStrings`,etc. 
`Int'` represents the datatype used in this repository to represent `Integers`. 

    data Int' = Pos Nat | Neg Nat 
    
`Neg n` represents `-(n+1)`. 

## Creation
There are 6 ways to create dimensions

    dimension :: forall a -> forall b. b -> Dimension (ValidParse @Symbol a)  b
    dimensions :: forall a -> forall f b. Functor f => f b -> f (Dimension (ValidParse @Symbol a) b)
    dimensionPoly :: forall a -> forall b.  b -> Dimension (ValidParse a) b 
    dimensionsPoly :: forall a -> forall f b. Functor f => f b -> f (Dimension (ValidParse a) b) 
    noParseDimension :: forall a -> forall b. b -> Dimension (Format a) b
    noParseDimensions :: forall a -> forall f b. Functor f => f b -> f (Dimension (Format a) b)

`dimension` and `dimensions` are the most common ones. They only work on symbols however, so if you want to use a different base, they would fail. `dimensions` is just dimension but lifted over a functor. Inspired by the ReadMe for the `Dimensional`  library. The polymorphic versions suffer from type ambiguity as the kind of the resulting `Dimension` is unknown. It is reccomended that you add a wrapper if you plan to use a seperate dimension. `noParseDimension` is if you don't want to use the built in parser and want to manually specify the dimensions. 
### Note on parser
The parser is very simple: 
it checks for `*`,and `/`, splits them into sections,
checks for `^` in the subsections and creates the dimensions accordingly.
As a result, all of the following are valid

    :k! Parse "*******" -> ['("", TI.Pos 1), '("", TI.Pos 1), '("", TI.Pos 1),'("", TI.Pos 1), '("", TI.Pos 1), '("", TI.Pos 1), '("", TI.Pos 1),'("", TI.Pos 1)]
    :k! Parse "*/*/***^201" -> ['("", TI.Pos 1), '("", TI.Pos 1), '("", TI.Neg 0),'("", TI.Pos 1), '("", TI.Neg 0), '("", TI.Pos 1), '("", TI.Pos 1),'("", TI.Pos 201)]
    :k! Parse "second/(meter*kilogram)"  -> [("kilogram),Pos 1),("(meter",Neg 0),("second",Pos 1)] 

`dimension` ensures that the invariants are upheld, but still allows annoyances. Check the types!
Given the annoyance inherent to type level coding, this may not change.

### Note on printer
The printer will not print the actual type as it is stored. e.g. 

    dimension "second/meter" 2 -> 2 second/meter

Despite the fact that the ordering of meter is actually before second, the printer tries to print the positive dimensions first. A useful note for debugging type errors.

## Multiplying,dision,etc.

`!+`,`!-`,`!*`,`!/`,`divD` can be used for multiplying and dividing dimensions. 
They are mostly just specialized forms of `liftD2` and `combineD2`. 
    
    
## Transformations along dimensions

```transform :: forall s t x a. TT.ToInt (LookupD0 s x) => (a -> a, a -> a) -> Dimension x a -> Dimension (Replace s t x) a```
This is used to completely switch a type parameter, whether it shows up in the positive or negative. Common usage would be with prefixes, `kilogram`  to `gram`,etc.

```transformpos :: forall s t x a. (TL.KnownNat (TI.ToNatural (LookupD0 s x))) => (a -> a) -> Dimension x a -> Dimension (Replace s t x) a```
Like `transform` but only needs the switch in the positive direction. However, it requries that the dimension only occurs a positive number of times. Useful when you know that your unit occurs in the positive place.

```apply :: forall x a. forall s -> TT.ToInt (LookupD0 s x) => (a -> a, a -> a) -> Dimension x a -> Dimension (Delete s x) a```
Like `transform`, but consumes the dimension. Can be useful with things like `billion`, or `mole`.

```applypos :: forall x a. forall s -> (TL.KnownNat (TI.ToNatural (LookupD0 s x))) => (a -> a) -> Dimension x a -> Dimension (Delete s x) a```
`apply` but only needs 1 function. I used this to eliminate the `billion` in the example.

```same :: forall s t x. (forall a. Dimension x a -> Dimension (Replace s t x) a)```
Assert that two things are the same, and replace one with another. Example: `g` `gram` `grams` all symbolize the same thing, but some places might use different ones.

```mkisos :: forall y x a. Dimension x a -> Dimension (Isos y x) a```
the same as repeated usage of `same`, uses a type level list.

## Extracting dimensions
Currently only one function, `undimension`, which requires that all tags be eliminated already.

## TODO:
This package is a work in progress, and I would appreciate help. I recently switched versions of GHC and changed many things to use `RequiredTypeArguments`, but the change is not yet finished, I also implemented a match class that allows for functions allong dimensional strings, creating futher extensibility. There are also very few tests. I'm quite bored of this project, but I am willing to finish it if someone shows interest.
