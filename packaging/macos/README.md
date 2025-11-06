This folder contains templates and automation to build, sign, package and upload the macOS app to the Mac App Store.

Overview

- `Info.plist` — template used by `scripts/build_macos_app.sh`.
- `entitlements.plist` — App Sandbox entitlements for Mac App Store.
- `scripts/build_macos_app.sh` — script to build Rust targets, create universal binary, assemble .app and produce a signed `.pkg`.
- `fastlane/Fastfile` — Fastlane lane `release_mac` that runs the build script and uploads with `deliver`.
- `.github/workflows/macos-release.yml` — GitHub Actions workflow to run the build+upload on `macos-latest`.

What you must provide
These operations require Apple developer credentials and signing identities. Add the following GitHub Secrets (recommended) in your repository Settings → Secrets & variables → Actions:

- `APP_SIGNING_P12` — Base64 of your Apple Distribution certificate export (`.p12`).
- `APP_SIGNING_P12_PASSWORD` — Password for the `.p12` file.
- `INSTALLER_P12` — Base64 of your "3rd Party Mac Developer Installer" certificate export (`.p12`) used to sign the installer.
- `INSTALLER_P12_PASSWORD` — Password for the installer `.p12`.
- `SIGN_IDENTITY` — The name of the signing identity (e.g. "Apple Distribution: Your Name (TEAMID)").
- `INSTALLER_IDENTITY` — The installer signing identity (e.g. "3rd Party Mac Developer Installer: Your Name (TEAMID)").
- `APP_STORE_CONNECT_API_KEY_JSON` — The contents of your App Store Connect API key `.p8` JSON file (base64 is acceptable, but the workflow writes it directly to a file).
- `APP_STORE_CONNECT_KEY_ID` — Key ID of the App Store Connect API key.
- `APP_STORE_CONNECT_ISSUER_ID` — Issuer ID of the App Store Connect API key.

Local run (macOS)

1. Ensure you have Xcode and Xcode command-line tools installed.
2. Install Rust and add macOS targets:

```bash
rustup target add x86_64-apple-darwin aarch64-apple-darwin
```

3. Import your signing certificates into your Keychain (or allow the script to import them from p12 files).

4. Set environment variables and run the script (example):

```bash
export APP_NAME="Time Warp"
export EXECUTABLE_NAME="time-warp"
export BUNDLE_ID="org.honeybadger.timewarp"
export VERSION="3.0.0"
export BUILD_VERSION="300"
export SIGN_IDENTITY="Apple Distribution: Your Name (TEAMID)"
export INSTALLER_IDENTITY="3rd Party Mac Developer Installer: Your Name (TEAMID)"

bash scripts/build_macos_app.sh
```

CI: GitHub Actions

- The workflow is at `.github/workflows/macos-release.yml` and is triggerable via the Actions UI (Workflow dispatch).
- Add the secrets listed above to your repo; the workflow will import the certificates into a temporary keychain and run `fastlane mac release_mac`.

Notes & caveats

- You must run the code-signing steps on macOS.
- Fastlane is included to handle the upload step; it uses the App Store Connect API key to authenticate. Ensure the API key has the appropriate role (App Manager or Admin) in App Store Connect.
- If you prefer to upload manually, the script will produce a signed `.pkg` in `dist/` which you can upload with Apple's Transporter app or Xcode Organizer.

Troubleshooting

- If the workflow fails during codesign, verify your certificates and team ID, and that the private key is correctly exported into the `.p12`.
- For notarization logs use `xcrun notarytool` with your API key.

If you want I can:

- Add a helper to convert PNG -> `.icns` and a placeholder `app.icns`.
- Add a `fastlane/Appfile` and `Deliverfile` tuned to your App Store metadata.
- Add a sample GitHub Actions secret creation guide and commands to export certs to base64 for upload.
