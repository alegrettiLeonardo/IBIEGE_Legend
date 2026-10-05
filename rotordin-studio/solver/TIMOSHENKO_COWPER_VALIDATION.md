# Timoshenko + Cowper lateral shaft qualification

Date: 2026-09-19

## Scope

This change replaces the active lateral shaft-element formulation with a single
complete cylindrical Timoshenko formulation using Cowper shear correction.

RotorDin retains its existing mesh/discretization responsibility in `predad`.
For native conical sections, `predad` already evaluates the effective section
at each finite-element midpoint. The shaft element therefore consumes the
effective element area `A=s(r)` and second moment `I=ie(r)`; it does not
reinterpret nodal diameter steps as an additional conical element.

The complete element consistently uses

`Phi = 12 E I / (kappa G A L^2)`

in stiffness, consistent mass (translational plus rotary inertia), and
gyroscopic matrices. The UMP contribution remains a separate distributed
negative-stiffness interpolation and is not derived from the physical mass
matrix.

The former active routines `coerig`, `coemas`, and `coegir` have been
removed from `matrizes.f`. There is no input selector: complete
Timoshenko+Cowper is now the only active lateral shaft formulation. Solver
revision is 2.1.0.

## Element-level qualification

Independent scalar equations were compared against the Fortran implementation
for 500 random circular/annular elements (seed 20260919), spanning
`Phi = 8.2059e-4 ... 1.95794e3`.

Maximum relative differences:

| quantity | maximum |
|---|---:|
| Cowper kappa | 3.65e-7 |
| Phi | 5.72e-7 |
| K Frobenius norm | 5.32e-7 |
| M Frobenius norm | 5.21e-7 |
| G Frobenius norm | 6.09e-7 |

Matrix identities were exact in the test arithmetic:

- `K = K^T`
- `M = M^T`
- `G = -G^T`

The slender-beam limit was additionally compared with the exact former legacy
routines. For legacy `Phi=4.992e-6` and Cowper `Phi=5.632e-6`, relative
matrix differences were approximately `4.69e-7 (K)`, `1.65e-6 (M)`, and
`5.73e-6 (G)`.

## UMP preservation

250 random UMP element cases were compared with the corrected legacy
translational Hermite reference, with the rotary-inertia contamination excluded.

Maximum Frobenius relative difference: `1.44e-7`.
Maximum symmetry residual: `0`.

## THIAGOK global matrix cross-check at 3581 rpm

Against an independent full-Timoshenko/Cowper assembly using the same physical
model and bearing interpolation:

| matrix | relative Frobenius difference |
|---|---:|
| M | 1.71e-8 |
| K total @3581 rpm | 1.36e-7 |
| G | 3.32e-8 |
| C @3581 rpm | within floating-point interpolation roundoff |

The THIAGOK shaft model spans approximately `Phi=2.1215 ... 374.2904` and
uses `kappa=0.8863636364` for the solid steel shaft sections with
`nu=0.3`.

## Critical-speed reference provenance

The previously carried Round-7 values `1996.822 / 2092.703 rpm` no longer
serve as the qualification oracle because the archived Round-7 package does not
contain the script/output that generated those two numbers.

A new reproducible independent reference was created from:

1. the archived THIAGOK effective mesh/disks/supports,
2. the original two speed-dependent bearing tables,
3. an independent implementation of RotorDin's local three-point Lagrange
   bearing interpolation,
4. independent full Timoshenko/Cowper M/K/G equations based on the formulation
   cross-checked against the pinned Petrobras ROSS source used in Round 7,
5. an independent state-space generalized eigenproblem, and
6. Brent root solution of the exact 1x crossing.

Reproducible independent 1x critical speeds:

- `1997.839577408863 rpm`
- `2092.968572741741 rpm`

RotorDin complete Timoshenko/Cowper:

- `1997.918457 rpm` — difference `+0.003948 %`
- `2093.016357 rpm` — difference `+0.002283 %`

The old carried numbers differ from the new reproducible reference by
approximately `-1.018 rpm` and `-0.266 rpm` and are marked superseded.

## THIAGOK response correlation at 3581 rpm

A fresh release recheck was performed against the independent Round-7
Timoshenko/Cowper reference.

Maximum observed differences:

| response quantity | maximum |
|---|---:|
| orbit component amplitude | 0.003841 % |
| orbit phase | 0.000794 deg |
| ellipse major/minor axis | 0.015150 % |
| bearing reaction component | 0.003811 % |
| bearing reaction phase | 0.000141 deg |

Whirl direction matched for every comparison point:
P1/P3 were BW, B1/B2 were FW, and both bearing-force orbits were FW.

## Literature / external benchmarks

After replacing the hybrid shaft formulation:

| benchmark | maximum difference |
|---|---:|
| Friswell 5.9.1 | 0.00469 % |
| Friswell 5.9.2 | 0.00548 % |
| Friswell 5.9.4 | 0.13873 % |
| RM12 critical speeds vs DyRoBeS | 0.02287 % |
| RM12 natural frequencies @2000 rpm vs DyRoBeS | 0.00288 % |
| RM12 unbalance peak speed vs DyRoBeS | 1.75e-6 % |
| RM12 unbalance peak amplitude vs DyRoBeS | 0.03247 % |

## Release / debug build gate

The complete solver was rebuilt with GNU Fortran 14.2.0 and system
LAPACK/BLAS. The debug build used runtime checking including
`-O0 -g -fcheck=all -fbacktrace -Wall -Wextra -Wimplicit-interface
-Wconversion -Wsurprising`.

THIAGOK completed without a runtime/bounds abort. Release and debug normalized
physical output were numerically identical; a fresh recheck also produced
byte-identical normalized output. The known IEEE underflow/denormal note in the
eigenproblem remains informational.

GitHub-hosted Actions attempted during feature development failed before any
workflow step was allocated on both Linux and Windows, so those runs are not
used as technical evidence for or against the implementation. Pull-request CI
should be rerun when runner provisioning is available.

## Gate decision

For the declared lateral shaft-element scope:

**COMPLETE TIMOSHENKO + COWPER FORMULATION: QUALIFIED FOR INTEGRATION**

**LEGACY HYBRID SHAFT ELEMENT: REMOVED FROM ACTIVE SOURCE**

This qualification does not by itself close unrelated outstanding audit
findings outside the shaft-element formulation.
