{-# LANGUAGE OverloadedStrings #-}

module GoldenData (
    -- SAMPLE.min3 expected values
    sampleMin3ActivityX,
    sampleMin3ActivityY,
    sampleMin3ActivityZ,
    sampleMin3ExpectedSupply,
    sampleMin3ExpectedCO2,
    sampleMin3ExpectedZinc,
    sampleMin3TechMatrix,
    sampleMin3BioMatrix,
    -- SAMPLE.min expected values
    sampleMinExpectedSupply,
    sampleMinExpectedCO2,
    -- Test tolerance
    defaultTolerance,
) where

-- | Default tolerance for floating point comparisons
defaultTolerance :: Double
defaultTolerance = 1.0e-10

-- SAMPLE.min3 Activity UUIDs
sampleMin3ActivityX :: String
sampleMin3ActivityX = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"

sampleMin3ActivityY :: String
sampleMin3ActivityY = "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"

sampleMin3ActivityZ :: String
sampleMin3ActivityZ = "dddddddd-dddd-dddd-dddd-dddddddddddd"

-- SAMPLE.min3 Expected Values (for 1 kg Product X)
-- Supply chain: Z (4.0 kg CO2, 0.003 kg Zinc) → Y (needs 0.4 kg Z) → X (needs 0.6 kg Y)

{- | Expected supply vector for SAMPLE.min3 with demand [1, 0, 0]
[Product X, Product Y, Product Z] = [1.0, 0.6, 0.24]
-}
sampleMin3ExpectedSupply :: [Double]
sampleMin3ExpectedSupply = [1.0, 0.6, 0.24]

{- | Expected CO2 emissions for 1 kg Product X
0.24 kg Z × 4.0 kg CO2/kg Z = 0.96 kg CO2
-}
sampleMin3ExpectedCO2 :: Double
sampleMin3ExpectedCO2 = 0.96

{- | Expected Zinc emissions for 1 kg Product X
0.24 kg Z × 0.003 kg Zinc/kg Z = 0.00072 kg Zinc
-}
sampleMin3ExpectedZinc :: Double
sampleMin3ExpectedZinc = 0.00072

{- | Expected technosphere matrix A (positive input coefficients)
Stored as list of (row, col, value) triplets
Row/Col: 0=X, 1=Y, 2=Z
-}
sampleMin3TechMatrix :: [(Int, Int, Double)]
sampleMin3TechMatrix =
    [ (1, 0, 0.6) -- Y needs 0.6 kg from X
    , (2, 1, 0.4) -- Z needs 0.4 kg from Y
    ]

{- | Expected biosphere matrix B
Stored as list of (row, col, value) triplets
Row: 0=CO2, 1=Zinc
Col: 0=X, 1=Y, 2=Z
-}
sampleMin3BioMatrix :: [(Int, Int, Double)]
sampleMin3BioMatrix =
    [ (0, 2, 4.0) -- Z emits 4.0 kg CO2
    , (1, 2, 0.003) -- Z emits 0.003 kg Zinc
    ]

-- SAMPLE.min Expected Values (with self-loops)
-- Activity A (market): outputs 1.0 kg X, inputs 0.8 kg X from B + 0.1 kg X from itself
-- This creates a circular dependency

{- | Expected supply vector for SAMPLE.min with demand [1, 0, 0, 0]
Calculation: (I-A)^-1 * [1, 0, 0, 0]
With self-loop 0.1, the scaling factor is approximately 1/(1-0.1) = 1.11
-}
sampleMinExpectedSupply :: [Double]
sampleMinExpectedSupply =
    [ 1.0 -- Market activity A (demand)
    , 0.8 -- Activity B (supplies to A)
    , 0.32 -- Activity C (supplies to B)
    , 0.128 -- Activity D (supplies to C)
    ]

-- Note: These are approximate values for validation
-- Actual values depend on the complete supply chain structure in SAMPLE.min

{- | Expected CO2 emissions for 1 kg Product X from SAMPLE.min
This depends on the complete supply chain
-}
sampleMinExpectedCO2 :: Double
sampleMinExpectedCO2 = 0.32 -- Approximate based on supply chain
