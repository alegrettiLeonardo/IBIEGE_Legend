# RotorDin Fortran solver embutido

Esta pasta contém o **backend científico oficial RotorDin em Fortran** usado pelo frontend PySide6.

O código científico é versionado **diretamente e de forma legível no Git**, em `solver/src/source/`. Não há snapshot `tar.xz`, chunks `.bin/.b64`, etapa de reconstrução ou árvore `.source` intermediária.

## Organização

```text
solver/
├── src/
│   └── source/
│       ├── rotordin.f
│       ├── entrada.f
│       ├── entrada_parts/
│       ├── matrizes.f
│       ├── foundation.f
│       ├── flexdisk.f
│       ├── model_audit.f
│       ├── progress.f
│       ├── transient.inc
│       ├── transient_*.f
│       ├── lapack/
│       └── torsion/
├── SOURCE_MANIFEST.sha256
├── build_solver.py
├── build/                 # gerado, não versionado
├── runtime/               # DLLs Windows geradas, não versionadas
└── rotordin(.exe)         # gerado, não versionado
```

O conteúdo de `solver/src/source/` foi importado do pacote RotorDin fornecido e é
protegido por manifesto. A árvore canônica atual possui **80 arquivos**:
**64 `.f`, 14 `.inc`, 1 `.txt` e 1 `.ini`**. Os nove includes adicionais em
`entrada_parts/` fazem parte do contrato atual de `entrada.f` e são versionados e
verificados como qualquer outro fonte científico.

O fingerprint SHA-256 determinístico da árvore (caminho + conteúdo) é:

```text
afe5e162168402b07df8960375fc45a58a094d15efde35380c199dfb27699d6d
```

Cada arquivo também possui hash individual em `SOURCE_MANIFEST.sha256`.

Esta revisão mantém os diagnósticos guiados do `MODEL_AUDIT` e acrescenta telemetria unificada de processamento em `progress.f`. Em `-std`, o solver preserva os resultados estruturados no `stdout` e emite no `stderr` eventos machine-readable do tipo:

```text
#PROGRESS ANALYSIS=MATRIX_ASSEMBLY EVENT=BEGIN TOTAL=... DETAIL=...
#PROGRESS ANALYSIS=MATRIX_ASSEMBLY EVENT=UPDATE PERCENT=... CURRENT=... TOTAL=... ...
#PROGRESS ANALYSIS=MATRIX_ASSEMBLY EVENT=STAGE DETAIL=...
#PROGRESS ANALYSIS=MATRIX_ASSEMBLY EVENT=END STATUS=OK
```

Em execução interativa/não-`-std`, o mesmo mecanismo usa a forma humana `[ANALYSIS] ...`. O frontend não recalcula percentuais nem inventa etapas: ele apenas interpreta e apresenta os eventos emitidos pelo Fortran.

## Verificação da fonte

```bash
python solver/build_solver.py --verify-source
```

A verificação falha se houver arquivo ausente, extra ou com conteúdo diferente do
manifesto, protegendo a rastreabilidade do solver científico sem esconder o código.

## Build do solver

```bash
python solver/build_solver.py --clean
```

No Linux/WSL são necessários `gfortran`, LAPACK e BLAS.

No Windows, o workflow usa GNU Fortran UCRT64/MSYS2 e OpenBLAS. O Python nativo
do runner chama o compilador UCRT64 e produz `solver/rotordin.exe`; as DLLs de
runtime necessárias são copiadas para `solver/runtime/`.

`model_audit.f` usa `-ffixed-line-length-none` de forma localizada para preservar as mensagens explicativas longas. Os demais fontes Fortran mantêm as flags históricas.

O CI recompila o solver diretamente de `solver/src/source/` e valida o manifesto,
o fingerprint da árvore, os testes com o RotorDin real e os bundles Linux/Windows.

## Relação com o frontend

O frontend **não replica a física do RotorDin**. `src/rotordin_frontend/solver.py`
serializa o modelo para o contrato de entrada, executa `rotordin`/`rotordin.exe`
e interpreta os relatórios. `AdvancedRotordinRunner` usa o mesmo executável para
Fundação, Transiente e Discos Flexíveis.

A janela `SolverTerminalDialog` consome a telemetria `#PROGRESS` durante a execução, atualiza o indicador de progresso/etapa e imprime no terminal a representação humana equivalente. O `stderr.log` continua preservando o texto original do solver para auditoria.

Os diagnósticos `MODEL_ERROR` continuam sendo apresentados de forma estruturada, sem reescrever ou inferir a física do solver.

Assim, a fonte da verdade fica separada de forma explícita:

- `solver/src/source/`: código científico Fortran;
- `src/rotordin_frontend/`: interface, persistência, serialização e apresentação;
- `tests/`: gates de integração entre frontend e solver real.
