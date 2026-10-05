import { describe, expect, it } from 'vitest';
import { demoProject } from '../data/demoProject';
import { setRotorStackAtomic, updateBearingAtomic, updateLegacyLoadAtomic, updateSegmentAtomic } from './edit';

describe('atomic project editing', () => {
  it('does not mutate the original project when a shaft edit is physically invalid', () => {
    const project = structuredClone(demoProject);
    const solid = project.segments.find((segment) => segment.sectionType === 'solid');
    if (!solid) throw new Error('fixture must contain a solid segment');

    const result = updateSegmentAtomic(project, solid.id, {
      sectionType: 'hollow',
      innerDiameterMm: solid.outerDiameterMm,
    });

    expect(result.accepted).toBe(false);
    expect(project.segments.find((segment) => segment.id === solid.id)?.innerDiameterMm).toBeUndefined();
    expect(result.issues.some((issue) => issue.code === 'SHAFT-HOLLOW-001')).toBe(true);
  });

  it('accepts a valid diameter edit as one transaction', () => {
    const project = structuredClone(demoProject);
    const solid = project.segments[0];
    const result = updateSegmentAtomic(project, solid.id, { outerDiameterMm: 195 });
    expect(result.accepted).toBe(true);
    expect(result.project.segments[0].outerDiameterMm).toBe(195);
    expect(project.segments[0].outerDiameterMm).not.toBe(195);
  });
});


describe('bearing and rotor-stack edits', () => {
  it('preserves a custom legacy bearing stiffness exactly instead of truncating it as VB6 abre_calculo does', () => {
    const project = structuredClone(demoProject);
    const bearing = project.bearings[0];
    const result = updateBearingAtomic(project, bearing.id, {
      coefficients: { kind: 'legacy-scalar', stiffnessNPerM: 3.75e9, infinite: false },
    });

    expect(result.accepted).toBe(true);
    const updated = result.project.bearings[0];
    expect(updated.coefficients.kind).toBe('legacy-scalar');
    if (updated.coefficients.kind !== 'legacy-scalar') throw new Error('unexpected bearing model');
    expect(updated.coefficients.stiffnessNPerM).toBe(3.75e9);
  });

  it('rejects a load edit that crosses a shaft step without mutating the original model', () => {
    const project = structuredClone(demoProject);
    const load = project.legacyLoads[1];
    const before = load.lengthMm;
    const result = updateLegacyLoadAtomic(project, load.id, { lengthMm: 200 });
    expect(result.accepted).toBe(false);
    expect(project.legacyLoads[1].lengthMm).toBe(before);
    expect(result.issues.some((issue) => issue.code === 'LEGACY-LOAD-STEP-001')).toBe(true);
  });

  it('stores rotor-stack identity explicitly and uniquely', () => {
    const project = structuredClone(demoProject);
    const target = project.legacyLoads[0];
    const result = setRotorStackAtomic(project, target.id);
    expect(result.accepted).toBe(true);
    expect(result.project.legacyLoads.filter((load) => load.isRotorStack)).toHaveLength(1);
    expect(result.project.legacyLoads.find((load) => load.isRotorStack)?.id).toBe(target.id);
  });
});
