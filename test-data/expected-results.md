# Expected Test Results - WITH UNIT CONVERSIONS

## Supply Chain Structure with Mixed Units
```
Steel Production (2.2 lb steel = 1.0 kg steel)
└── Electricity (7.2 MJ = 2.0 kWh) 
    └── Coal Extraction (2.2 lb coal = 1.0 kg coal total)
```

## Unit Conversion Factors Used:
- **Energy**: 1 kWh = 3.6 MJ  
- **Mass**: 1 kg = 2.2 lb
- **Emissions**: All emissions stay in kg (standard biosphere unit)

## Manual Calculation for Steel Production Inventory

### Step 1: Convert steel production inputs to standard units
- Input: 7.2 MJ electricity → 7.2 ÷ 3.6 = **2.0 kWh**
- Output: 2.2 lb steel → 2.2 ÷ 2.2 = **1.0 kg steel**

### Step 2: Direct Emissions from Steel Production:
- CO2: 1.8 kg (direct emission - already in kg)

### Step 3: Indirect Emissions from Electricity (2.0 kWh):
- CO2: 2.0 kWh × 0.9 kg/kWh = **1.8 kg**
- SO2: 2.0 kWh × 0.01 kg/kWh = **0.02 kg**

### Step 4: Calculate coal needed for electricity
- Electricity needs: 2.0 kWh  
- Coal per kWh: 1.1 lb = 0.5 kg per kWh
- Total coal needed: 2.0 kWh × 0.5 kg/kWh = **1.0 kg coal**

### Step 5: Indirect Emissions from Coal Extraction (1.0 kg coal):
- CO2: 1.0 kg coal × 0.1 kg CO2/kg coal = **0.1 kg**

## Expected Final Inventory for 2.2 lb Steel (= 1.0 kg):
- **CO2**: 1.8 + 1.8 + 0.1 = **3.7 kg**  
- **SO2**: **0.02 kg**

## Unit Conversion Tests to Verify:

1. **Energy Conversion**: 7.2 MJ → 2.0 kWh (÷ 3.6)
2. **Mass Conversion**: 2.2 lb → 1.0 kg (÷ 2.2)  
3. **Coal Scaling**: 1.1 lb/kWh → 0.5 kg/kWh
4. **Supply Chain Units**: Mixed units properly converted throughout tree
5. **Inventory Units**: Final results in standard kg units

## Critical Test: Unit Consistency
The engine must:
- ✅ Convert 7.2 MJ input to 2.0 kWh for calculations
- ✅ Convert 2.2 lb outputs to 1.0 kg for scaling  
- ✅ Handle 1.1 lb coal input → 0.5 kg for emission factors
- ✅ Produce final inventory in standard kg units
- ✅ Maintain mass/energy balance across conversions

## API Test Commands:
```bash
# Test with mixed units
cabal run volca -- --data test-data --root steel-prod-test-0001 --method dummy --server

# Verify tree structure handles unit conversions
curl "http://localhost:8080/api/v1/activity/steel-prod-test-0001/tree"

# Check final inventory (should be same 3.7 kg CO2, 0.02 kg SO2)
curl "http://localhost:8080/api/v1/activity/steel-prod-test-0001/inventory"
```

**Key Test**: Final inventory results should be **identical** to single-unit version, proving unit conversion system works correctly!