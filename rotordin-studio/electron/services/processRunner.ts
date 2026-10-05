import { spawn } from 'node:child_process';

export interface ProcessRunRequest {
  executable: string;
  args: string[];
  cwd: string;
  timeoutMs: number;
  stdinText?: string;
  env?: NodeJS.ProcessEnv;
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
      env: request.env ?? process.env,
      stdio: [request.stdinText == null ? 'ignore' : 'pipe', 'pipe', 'pipe'],
    });

    let stdout = '';
    let stderr = '';
    let timedOut = false;

    if (request.stdinText != null && child.stdin) {
      child.stdin.setDefaultEncoding('utf8');
      child.stdin.end(request.stdinText);
    }

    if (!child.stdout || !child.stderr) {
      reject(new Error('RUN-PROCESS-001: Solver stdout/stderr pipes were not created.'));
      return;
    }
    const stdoutStream = child.stdout;
    const stderrStream = child.stderr;
    stdoutStream.setEncoding('utf8');
    stderrStream.setEncoding('utf8');
    stdoutStream.on('data', (chunk: string) => { stdout += chunk; });
    stderrStream.on('data', (chunk: string) => { stderr += chunk; });

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
