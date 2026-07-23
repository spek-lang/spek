# Security Policy

Spek is in **pre-1.0 development**. Security fixes target the latest `main`; there
are no supported older release lines yet.

## Reporting a vulnerability

Please report security issues **privately**. Do not open a public issue for a
vulnerability.

- Once the repository is public, use GitHub's **private vulnerability reporting**
  (the *Security* tab → *Report a vulnerability*).
- Until then, contact the maintainers directly.
  <!-- TODO before public release: set a security contact (email or GitHub
       Security Advisories) here. -->

We'll acknowledge the report, work on a fix, and coordinate disclosure with you.

## Known limitations (not vulnerabilities, but read these)

These are documented design states of the current pre-1.0 code, not bugs:

- **The cluster TCP transport is not encrypted or authenticated.**
  `Spek.Cluster.Tcp` sends frames in plaintext. The `ClusterSharedKey` option is
  **not yet enforced**: it does not authenticate peers today (it logs a warning).
  TLS / mTLS and shared-secret handshake are planned but not implemented. **Run
  clusters only on a trusted network** (private subnet, VPN, or loopback) until
  the wire is secured. See `Spek.Cluster.Tcp/README.md`.
- **`interop using` and `CE0080`.** Spek blocks hostile namespace imports
  (reflection, `Unsafe`, marshalling) by default; `interop using` opts back in and
  is flagged with `CE0086`. Code that opts in is outside Spek's safety guarantees
  by design.

If you find a way to *bypass* a guarantee Spek claims to enforce at compile time
(e.g. mutate a message after sending without a diagnostic, or share mutable state
across actors undetected), that **is** a security-relevant correctness issue.
Please report it via the private channel above.
