import type { RotorDinDesktopApi } from '../../electron/contracts';

declare global {
  interface Window {
    rotorDinDesktop?: RotorDinDesktopApi;
  }
}

export {};
