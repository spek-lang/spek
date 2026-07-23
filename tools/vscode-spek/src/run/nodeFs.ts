// The real-file-system ProjectFs adapter. Kept out of projectResolver.ts so
// the resolver stays purely functional; tests use either this adapter (the
// real-tree elevators proof) or an in-memory fake.

import * as fs from 'fs';
import { ProjectFs } from './projectResolver';

export const nodeFs: ProjectFs = {
  listDir(dir: string): string[] {
    try {
      return fs.readdirSync(dir);
    } catch {
      return [];
    }
  },
  isDirectory(p: string): boolean {
    try {
      return fs.statSync(p).isDirectory();
    } catch {
      return false;
    }
  },
  readFile(file: string): string | null {
    try {
      return fs.readFileSync(file, 'utf8');
    } catch {
      return null;
    }
  },
};
