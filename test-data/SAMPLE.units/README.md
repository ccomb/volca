# SAMPLE.units - Unit Conversion Test Dataset

This test dataset validates unit conversion functionality and prevents regressions like the 1M scaling factor bug.

## Test Activities

### 1. steel_production_tonnes.spold
**Purpose**: Mass unit conversion testing
**Units tested**: t (tonnes) ↔ kg ↔ g ↔ metric ton
**Key validation**:
- Output: 1.0 t steel
- Input: 1600.0 kg iron ore (should equal 1.6 t)
- Input: 0.5 t coal
- Emissions: 2000.0 kg CO2, 50000.0 g particulates

### 2. electricity_generation_energy.spold
**Purpose**: Energy unit conversion testing
**Units tested**: MJ ↔ kWh ↔ GJ ↔ kJ ↔ TJ
**Key validation**:
- Output: 1.0 MJ electricity
- Input: 0.833 kWh coal energy (should equal ~3.0 MJ)
- Input: 0.002 GJ heat (should equal 2.0 MJ)
- Input: 100.0 kJ steam (should equal 0.1 MJ)

### 3. nuclear_power_activity.spold
**Purpose**: Activity unit (radioactivity) conversion testing
**Units tested**: kBq ↔ Bq
**Key validation**:
- Output: 1.0 MJ electricity
- Emissions: 0.000001 kBq Radon-222, 50.0 Bq Cesium-137 (should be 0.05 kBq)
- Critical test case for the fixed scaling bug!

### 4. agriculture_compound.spold
**Purpose**: Compound unit conversion testing
**Units tested**: person*km, m2*year
**Key validation**:
- Tests transport and land use compound units
- Its per-hectare and per-kilogram rows (`kg/ha`, `MJ/kg`, `kg N/ha`, `kg P/ha`)
  are parameter units, which the table does not carry and nothing converts:
  they exercise the warning path, not a conversion

### 5. water_treatment_volume.spold
**Purpose**: Volume, area, and length unit testing
**Units tested**: m³ ↔ l ↔ ml ↔ dm³, km, ha
**Key validation**:
- Output: 1.0 m³ treated water
- Input: 1100.0 l raw water (should equal 1.1 m³)
- Input: 50.0 ml coagulant (should equal 0.00005 m³)

### 6. unknown_units_test.spold
**Purpose**: Unknown unit graceful handling
**Units tested**: custom_unit, qubit, kg*m/s²*°C, mystery_unit, empty unit
**Key validation**:
- Engine should not crash on unknown units
- Should warn about unknown units but continue processing
- Should handle empty unit names gracefully

## Expected Conversion Factors

### Mass (reference: kg)
- 1 t = 1000 kg
- 1 g = 0.001 kg
- 1 metric ton = 1000 kg

### Energy (reference: MJ)
- 1 kWh = 3.6 MJ
- 1 GJ = 1000 MJ
- 1 kJ = 0.001 MJ
- 1 TJ = 1000000 MJ

### Volume (reference: m³)
- 1 l = 0.001 m³
- 1 ml = 0.000001 m³
- 1 dm³ = 0.001 m³

### Activity (reference: kBq)
- 1 kBq = 1 kBq
- 1 Bq = 0.001 kBq

### Area (reference: m²)
- 1 ha = 10000 m²

### Length (reference: m)
- 1 km = 1000 m

## Testing Commands

```bash
# Test individual activities
./run.sh --data SAMPLE.units activity steel-prod-uuid
./run.sh --data SAMPLE.units activity nuclear-power-uuid

# Test unit conversion warnings
./run.sh --data SAMPLE.units activity unknown-units-uuid

# Test inventory calculations with mixed units
./run.sh --data SAMPLE.units inventory steel-prod-uuid
```

## Expected Warnings

The unknown_units_test should generate warnings like:
```
[WARNING] Unknown unit encountered: custom_unit
[WARNING] Unknown unit encountered: qubit
[WARNING] Unknown unit encountered: kg*m/s²*°C
[WARNING] Unknown unit encountered: mystery_unit
```

## Regression Prevention

This dataset specifically prevents:
1. **Double normalization bugs** - mixing normalized and raw values
2. **Unit scaling errors** - incorrect conversion factors
3. **Reference product normalization errors** - improper matrix scaling
4. **Unknown unit crashes** - engine should handle gracefully
5. **Compound unit parsing issues** - complex unit expressions

## Matrix Assembly Validation

Key matrix relationships to verify:
- All technosphere inputs normalized per unit of reference product
- Biosphere flows properly scaled by reference product amounts
- Unit conversions applied before matrix assembly, not after
- No double normalization in matrix triplet construction