import { spawn } from 'node:child_process';

export interface ProcessRunRequest {
  executable: string;
  args: string[];
  cwd: string;
  timeoutMs: number;
  /**
   * Solver execution remains gated until a solver-specific Golden Master is
   * frozen. Callers must provide the qualification identifier explicitly.
   */
  qualificationId: string;
}

export interface ProcessRunResult {
  exitCode: number | null;
  signal: NodeJS.Signals | null;
  stdout: string;
  stderr: string;
  timedOut: boolean;
}

/**
 * Safe replacement for legacy Module1.ExecCmd.
 *
 * Differences are intentional:
 * - no command-line string concatenation;
 * - shell=false;
 * - launch errors are surfaced;
 * - stdout/stderr are captured;
 * - timeout terminates the child;
 * - exit code is returned.
 *
 * This service is deliberately not exposed through preload yet. Solver-specific
 * adapters must pass their Golden Master qualification gate first.
 */
export async function runQualifiedProcess(request: ProcessRunRequest): Promise<ProcessRunResult> {
  if (!request.qualificationId.trim()) {
    throw new Error('RUN-QUAL-001: Solver execution requires a qualification identifier.');
  }

  return new Promise<ProcessRunResult>((resolve, reject) => {
    const child = spawn(request.executable, request.args, {
      cwd: request.cwd,
      shell: false,
      windowsHide: true,
      stdio: ['ignore', 'pipe', 'pipe'],
    });

    let stdout = '';
    let stderr = '';
    let timedOut = false;

    child.stdout.setEncoding('utf8');
    child.stderr.setEncoding('utf8');
    child.stdout.on('data', (chunk: string) => { stdout += chunk; });
    child.stderr.on('data', (chunk: string) => { stderr += chunk; });

    const timer = setTimeout(() => {
      timedOut = true;
      child.kill();
    }, request.timeoutMs);

    child.once('error', (error) => {
      clearTimeout(timer);
      reject(error);
    });

    child.once('close', (exitCode, signal) => {
      clearTimeout(timer);
      resolve({ exitCode, signal, stdout, stderr, timedOut });
    });
  });
}
