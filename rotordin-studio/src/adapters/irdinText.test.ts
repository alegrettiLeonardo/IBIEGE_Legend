import { describe, expect, it } from 'vitest';
import { legacyCompatibilityNotes, validateProject } from '../domain';
import { decodeIrDinBytes, importIrDinText, parseIrDinDocument } from './irdinText';

const fixture = [
  '[irDin]',
  'Data=18/09/2026 13:30:05',
  'Usuario=ANGELO',
  '[Dados]',
  'ref=EST',
  'comp=ST41_1000_B3_60HZ_1675_63536',
  'nnom=1800',
  's_melast=207000000000',
  's_masesp=7850',
  's_poisson=0.3',
  'c_rpmi=0',
  'c_rpmf=3000',
  'c_div=25',
  '[Secoes]',
  '1,0=70',
  '1,1=570',
  '2,0=100',
  '2,1=390',
  '2,2=620',
  '2,3=127',
  '2,6=4',
  '2,7=60',
  '[Massas]',
  '1,0=70',
  '1,1=100',
  '1,2=1000',
  '1,3=620',
  '1,4=1',
  '1,5=0',
  '[Mancais]',
  '1,0=50',
  '1,10=_bearing_a',
  '1,11=TABLE§500|1|2|3|4|5|6|7|8§850|11|12|13|14|15|16|17|18',
  '2,0=150',
  '2,1=1000',
  '2,2=2000',
  '[Desbal]',
  '1,0=80',
  '1,1=0',
  '1,2=110175.3',
  '[Respo]',
  '1,0=50',
  '1,1=1',
  '2,0=150',
  '2,1=2',
  '[Concent]',
  '[Suporte]',
  '1,0=1',
  '1,1=2730000000',
  '1,2=3410000000',
  '1,9=415',
  '[Fim]',
].join('\r\n');

describe('current irDin text importer', () => {
  it('decodes the ISO-8859-1 section-sign delimiter used by production bearing tables', () => {
    const bytes = Uint8Array.from([84, 65, 66, 76, 69, 0xa7, 53, 48, 48]);
    expect(decodeIrDinBytes(bytes)).toBe('TABLE§500');
  });

  it('parses INI-like sections without destroying indexed keys', () => {
    const raw = parseIrDinDocument(fixture);
    expect(raw.sections.Dados.ref).toBe('EST');
    expect(raw.sections.Secoes['2,7']).toBe('60');
  });

  it('imports combined ribbed+hollow geometry natively and keeps legacy BIEGE incompatibility explicit', () => {
    const result = importIrDinText(fixture);
    expect(result.project.segments).toHaveLength(2);
    const segment = result.project.segments[1];
    expect(segment.ribbed?.packageDiameterMm).toBe(620);
    expect(segment.innerDiameterMm).toBe(60);

    expect(validateProject(result.project).valid).toBe(true);
    expect(legacyCompatibilityNotes(result.project).some((issue) => issue.code === 'LEGACY-SHAFT-003')).toBe(true);
  });

  it('imports speed-dependent bearing tables in the supplied current UI coefficient order', () => {
    const result = importIrDinText(fixture);
    const bearing = result.project.bearings[0];
    expect(bearing.coefficients.kind).toBe('matrix');
    if (bearing.coefficients.kind !== 'matrix') throw new Error('unexpected model');
    expect(bearing.coefficients.table).toHaveLength(2);
    expect(bearing.coefficients.table?.[0]).toMatchObject({
      speedRpm: 500,
      stiffness: { xx: 1, xz: 2, zx: 3, zz: 4 },
      damping: { xx: 5, xz: 6, zx: 7, zz: 8 },
    });
  });

  it('imports masses, unbalance, response points and supports', () => {
    const result = importIrDinText(fixture);
    expect(result.project.legacyLoads[0].isRotorStack).toBe(true);
    expect(result.project.forces[0]).toMatchObject({ positionMm: 80, magnitude: 110175.3, unit: 'g-mm' });
    expect(result.project.responsePoints.map((item) => item.coordinate)).toEqual(['X', 'Z']);
    expect(result.project.supports[0]).toMatchObject({
      locationMm: 50,
      massKg: 415,
      stiffness: { xx: 2730000000, zz: 3410000000, xz: 0, zx: 0 },
    });
  });
});
