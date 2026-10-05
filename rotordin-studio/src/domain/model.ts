export type EntityId = string;

export type SegmentSectionType = 'solid' | 'hollow' | 'ribbed';

export interface RibbedSection {
  packageDiameterMm: number;
  ribThicknessMm: number;
  hammerThicknessMm: number;
  hammerWidthMm: number;
  ribCount: number;
}

export interface ShaftSegment {
  id: EntityId;
  sectionType: SegmentSectionType;
  lengthMm: number;
  outerDiameterMm: number;
  /**
   * Legacy D_F. Undefined means a cylindrical segment.
   * The VB6 code treats conicity independently from solid/hollow/ribbed type,
   * so the target model preserves that orthogonality.
   */
  endOuterDiameterMm?: number;
  innerDiameterMm?: number;
  ribbed?: RibbedSection;
}

export interface Matrix2x2 {
  xx: number;
  xz: number;
  zx: number;
  zz: number;
}

export interface BearingCoefficientSample {
  speedRpm: number;
  stiffness: Matrix2x2;
  damping: Matrix2x2;
}

export type BearingCoefficientModel =
  | {
      kind: 'legacy-scalar';
      stiffnessNPerM: number;
      infinite: boolean;
    }
  | {
      kind: 'matrix';
      stiffness: Matrix2x2;
      damping: Matrix2x2;
      source: 'constant' | 'file' | 'speed-dependent' | 'support';
      sourceFile?: string;
      table?: BearingCoefficientSample[];
      /** Exact imported TABLE§... payload for byte-stable native serialization. */
      rawTableContent?: string;
    };

export interface Bearing {
  id: EntityId;
  name: string;
  positionMm: number;
  coefficients: BearingCoefficientModel;
  supportId?: EntityId;
}

export interface LegacyMassLoad {
  id: EntityId;
  startMm: number;
  /**
   * Legacy LC. Zero is a concentrated load/mass in exporta_biege.
   */
  lengthMm: number;
  massKg: number;
  magneticThrust: boolean;
  /**
   * Extension required by the React model. The VB6 stores this only as
   * MSFlexGrid CellBackColor and does not persist it to d_carbiege.
   */
  isRotorStack: boolean;
  outerDiameterMm?: number;
  innerDiameterMm?: number;
  umpEnabled?: boolean;
}

export interface ConcentratedMass {
  id: EntityId;
  positionMm: number;
  massKg: number;
  ix?: number;
  iy?: number;
  iz?: number;
}

export interface ForceExcitation {
  id: EntityId;
  positionMm: number;
  phaseDeg: number;
  magnitude: number;
  unit: 'N' | 'g-mm';
  type: 'synchronous' | 'unbalance' | 'custom';
  direction: 'X' | 'Y' | 'Z' | 'radial' | 'axial';
}

export interface ResponsePoint {
  id: EntityId;
  positionMm: number;
  coordinate: 'X' | 'Y' | 'Z';
  orientationDeg: number;
  note?: string;
  sourceCoordinateCode?: number;
}

export interface Support {
  id: EntityId;
  name: string;
  type: string;
  locationMm: number;
  stiffness: Matrix2x2;
  damping: Matrix2x2;
  massKg?: number;
}

export type AnalysisKind =
  | 'campbell'
  | 'modes'
  | 'unbalance-response'
  | 'elastic-line'
  | 'critical-speed-map'
  | 'orbit'
  | 'stability';

export interface AnalysisSelection {
  kind: AnalysisKind;
  enabled: boolean;
}

export interface RotorProjectSettings {
  material?: {
    youngsModulusPa?: number;
    densityKgPerM3?: number;
    poissonRatio?: number;
    diskDensityKgPerM3?: number;
  };
  speed?: {
    nominalRpm?: number;
    initialRpm?: number;
    finalRpm?: number;
  };
  discretization?: {
    divisions?: number;
    maxDivisions?: number;
    relativeLoadLimit?: number;
    packageDivisions?: number;
  };
  campbell?: {
    initialRpm?: number;
    finalRpm?: number;
    divisions?: number;
    rotations?: number;
    interpolationPoints?: number;
  };
  unbalanceResponse?: {
    initialRpm?: number;
    finalRpm?: number;
    divisions?: number;
    modes?: number;
  };
  modes?: {
    modes?: number;
  };
  elasticLine?: {
    inclinationDeg?: number;
    gravityMPerS2?: number;
  };
  criticalSpeedMap?: {
    initialStiffnessNPerM?: number;
    divisions?: number;
  };
  graphics?: {
    scale?: number;
    x0?: number;
    responseCurves?: number;
    modes?: number;
    maps?: number;
    logDecrementCurves?: number;
  };
}

export interface RotorProject {
  id: EntityId;
  reference: string;
  description: string;
  component?: string;
  line?: string;
  frame?: string;
  poles?: number;
  frequencyHz?: number;
  nominalSpeedRpm?: number;
  user?: string;
  settings?: RotorProjectSettings;
  segments: ShaftSegment[];
  bearings: Bearing[];
  legacyLoads: LegacyMassLoad[];
  concentratedMasses: ConcentratedMass[];
  forces: ForceExcitation[];
  responsePoints: ResponsePoint[];
  supports: Support[];
  analyses: AnalysisSelection[];
  generateBiegePlot: boolean;
}

export interface ValidationIssue {
  code: string;
  severity: 'error' | 'warning';
  message: string;
  entityId?: EntityId;
  legacySource?: string;
}

export interface ValidationResult {
  valid: boolean;
  issues: ValidationIssue[];
}

export interface LegacyImportResult<T> {
  value: T;
  issues: ValidationIssue[];
}
