# RotorDin Studio UI

React + TypeScript implementation of the RotorDin Studio engineering desktop UI concept, created on top of the legacy IBIEGE repository without modifying the VB6 application.

## Implemented screens

- Project Overview
- Shaft Modeler
- Bearings & Supports
- Masses & Excitations
- Analysis Plan
- Results Workspace
- Runs & Reports
- Settings & Integrations

The branch is intentionally UI-first. It uses mock engineering data and does **not** yet execute BIEGE/FLECHA/FDE/RotorDin solvers or access the legacy database. Those integrations should be implemented behind typed adapters after Golden Master characterization of the legacy system.

## Run locally

```bash
cd rotordin-studio
npm install
npm run dev
```

## Build

```bash
npm run build
```

## Design principles

- Engineering state is represented explicitly in React state/domain data rather than colors or control positions.
- Shaft geometry is rendered as SVG and is a derived view only.
- Units are visible in editors and result views.
- Analysis configuration, results and run traceability are separated into dedicated workspaces.
- Navigation and major interactions are functional in this UI prototype.

## Next implementation gates

1. Freeze legacy solver/binary authority and Golden Masters.
2. Extract typed domain models and Zod validators from the UI mock data.
3. Add Electron shell with `contextIsolation=true` and a typed preload API.
4. Implement project persistence adapter.
5. Implement per-run isolated workspace and solver adapters.
6. Qualify UI-to-solver serialization against legacy outputs.
