import { describe, expect, it } from 'vitest';
import { demoProject } from '../data/demoProject';
import { updateSegmentAtomic } from './edit';

describe('atomic project editing', () => {
  it('does not mutate the original project when a shaft edit violates a legacy rule', () => {
    const project = structuredClone(demoProject);
    const solid = project.segments.find((segment) => segment.sectionType === 'solid');
    if (!solid) throw new Error('fixture must contain a solid segment');

    const result = updateSegmentAtomic(project, solid.id, {
      sectionType: 'hollow',
      innerDiameterMm: 20,
    });

    expect(result.accepted).toBe(false);
    expect(project.segments.find((segment) => segment.id === solid.id)?.sectionType).toBe('solid');
    expect(result.issues.some((issue) => issue.code === 'LEGACY-SHAFT-002')).toBe(true);
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
