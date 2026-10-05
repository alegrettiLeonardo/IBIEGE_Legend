# A9 — Current irDin project import and production-case qualification

## Evidence used

This block uses two kinds of evidence:

1. the audited `IBIEGE_Legend` VB6 source for legacy compatibility constraints;
2. the user-supplied current irDin case
   `EST-ST41_1000_B3_60HZ_1675_63536`.

The current irDin parser source is **not present** in `IBIEGE_Legend`. Therefore fields that are not unambiguously supported by the supplied file plus the current UI screenshots remain explicitly marked as provisional instead of being silently invented.

The production case is stored as a UTF-8 semantic fixture under:

`tests/fixtures/EST-ST41_1000_B3_60HZ_1675_63536.irdin.txt`

The original user upload is ISO-8859-1. The Electron importer reads legacy text bytes using Windows-1252/Latin-1-compatible decoding so the section-sign delimiter in bearing tables is preserved.

## Project identity

The supplied file provides:
- `ref=EST`
- `comp=ST41_1000_B3_60HZ_1675_63536`
- `nnom=1800`
- global speed range `0..2160 rpm`

The importer does not infer frame, line, poles or frequency from the component name.

## Analysis settings

Known `[Dados]` mappings implemented:

### Material
- `s_melast` → Young's modulus
- `s_masesp` → shaft density
- `s_poisson` → Poisson ratio
- `d_masesp` → disk density

### Shaft/model discretization
- `s_div`
- `s_mxdiv`
- `s_relld`
- `p_div`

### Campbell
- `c_rpmi`
- `c_rpmf`
- `c_div`
- `c_nrrot`
- `c_interp`

### Unbalance response
- `d_rpmi`
- `d_rpmf`
- `d_div`
- `d_nrmodos`

### Modes
- `m_nrmodos`

### Elastic line
- `le_inc`
- `le_g`

### Critical-speed map
- `mr_rini`
- `mr_nrdc`

### Graphics
- `gr_esc`
- `gr_x0`
- `gr_rcp`
- `gr_mod`
- `gr_map`
- `gr_log`

The file does not show explicit enabled/disabled analysis flags. Imported parameters are preserved, while all analysis selections default to disabled and emit `IRDIN-ANALYSIS-FLAGS-001`.

## [Secoes]

The current UI screenshots show the same indexed columns used by the current file:

| index | meaning |
|---|---|
| 0 | L |
| 1 | D |
| 2 | DPCT |
| 3 | A |
| 4 | B |
| 5 | C |
| 6 | NC |
| 7 | D_INT |
| 8 | D_F |

### Important discovery: current irDin supports geometry that old IBIEGE cannot

In the supplied production case, section 9 has:
- DPCT = 620
- A = 127
- NC = 4
- D_INT = 60

Therefore that section is simultaneously **ribbed and hollow**.

This is incompatible with the old `frmMain.IncSgmnt/AltSgmnt` rule, which globally forbids hollow and ribbed shaft representations together.

A9 changes the architecture accordingly:

- **native RotorDin Studio validation allows orthogonal ribbed + hollow geometry**;
- **IBIEGE/BIEGE14 compatibility validation still rejects it**;
- BIEGE14 serialization is blocked instead of dropping either ST or HL information.

This supersedes the earlier assumption that the old IBIEGE restriction should be a global target-domain restriction.

## [Massas]

Current UI + production-file mapping:

| index | meaning |
|---|---|
| 0 | Xi |
| 1 | LC |
| 2 | Kg |
| 3 | DExt |
| 4 | Pacote |
| 5 | UMP |
| 6 | DInt when present |

The production case has three mass regions. Row 1 is explicitly marked as the rotor package.

Unlike old IBIEGE/FLECHA, the current irDin file **does persist** package identity. The importer therefore preserves `isRotorStack` directly.

## [Mancais]

Current UI shows indexed columns for:
- position;
- Kxx, Kzz, Kxz, Kzx;
- Cxx, Czz, Cxz, Czx;
- type;
- file;
- embedded table.

The supplied production case contains two bearing files and two embedded `TABLE§...` definitions.

Each embedded table has 11 speed rows:
- 500 rpm
- 850 rpm
- ...
- 4000 rpm

The table parser preserves:
- speed;
- full 2x2 K matrix;
- full 2x2 C matrix;
- source file name;
- all table points.

The eight-coefficient table mapping follows the column order visible in the supplied current RotorDin UI:

`Kxx,Kxz,Kzx,Kzz,Cxx,Cxz,Czx,Czz`

The current Fortran `entrada.f90`/`beatab` contract is now the authority: TABLE rows are `rpm,kxx,kxz,kzx,kzz,cxx,cxz,czx,czz[,kph,kth]`. This differs from the legacy UI grid order and is intentionally handled separately.

## [Desbal]

Mapped from current UI and file:

- column 0 → position
- column 1 → phase
- column 2 → unbalance value

Imported as `g-mm` unbalance excitation.

## [Respo]

The production file uses coordinate codes 1 and 2.

The current IBIEGE repository does not contain the irDin parser that defines these codes.

The Fortran output contract in `saidas.f90` documents `desp=1` as horizontal and `desp=2` as vertical; the native lateral DOFs are `x,z`. The importer therefore maps:
- 1 → X
- 2 → Z

and still stores the original numeric code in `sourceCoordinateCode` for auditability.

## [Concent]

Importer supports current UI columns:
- Xi
- kg
- Ix
- Iy
- Iz

The supplied case has no concentrated-mass rows.

## [Suporte]

Current UI + production-file mapping:

- column 0 → linked bearing number (`Man`)
- 1..4 → Kxx,Kzz,Kxz,Kzx
- 5..8 → Cxx,Czz,Cxz,Czx
- 9 → support mass
- 10 → optional type

The production case imports two supports with:
- Kxx = 2.73e9
- Kzz = 3.41e9
- mass = 415 kg

Support location is resolved from its linked bearing position.

## Encoding

The original production upload is ISO-8859-1, not UTF-8.

This matters because the embedded bearing table uses byte `0xA7` (`§`) as a row delimiter.

The desktop importer therefore:
1. reads raw bytes;
2. decodes irDin `.txt` using Windows-1252/Latin-1-compatible decoding;
3. only then parses sections and bearing tables.

A unit test explicitly covers `0xA7 → §`.

## Save safety

Imported irDin `.txt` is treated as a compatibility source, not a native writable project.

After import:
- the source file is not assigned as the native save target;
- the project becomes unsaved;
- Save creates/prompts for a `.rdin.json` project;
- the React application will not overwrite the original irDin text with JSON.

## Production fixture qualification

Automated checks verify the supplied case imports as:

- reference: EST
- component: ST41_1000_B3_60HZ_1675_63536
- nominal speed: 1800 rpm
- 17 shaft sections
- 3 mass regions
- 2 bearings
- 11 speed-dependent coefficient points per bearing
- 2 unbalance excitations
- 4 response points
- 2 supports
- support mass 415 kg
- combined ribbed+hollow section preserved

## Implementation

- `src/adapters/irdinText.ts`
- `src/adapters/irdinText.test.ts`
- `src/adapters/irdinProductionFixture.test.ts`
- `tests/fixtures/EST-ST41_1000_B3_60HZ_1675_63536.irdin.txt`
- Electron `projectFiles.ts` import path
- React project header, shaft, bearing and analysis pages now consume imported project/settings data.

## Next gate

Before direct current RotorDin solver execution is enabled:

1. obtain/audit current irDin parser or solver input source;
2. confirm `[Mancais]` embedded table coefficient order;
3. confirm `[Respo]` coordinate-code meaning;
4. identify the current solver executable/input/output contract;
5. freeze at least one current irDin solver result for the EST-ST41 case;
6. only then expose the qualified current RotorDin process runner through preload.
