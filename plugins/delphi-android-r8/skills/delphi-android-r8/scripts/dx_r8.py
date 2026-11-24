#!/usr/bin/env python3
"""dx_r8.py - R8 for Delphi FMX Android builds (RAD Studio 13.x).

Tested end to end on a Delphi 13.1 FMX app with AdMob + UMP (Scary Recipes 2.0.1, Nov 2026)
in the "shrink" + "bundle-metadata" path driven by DX.R8.targets.

  shrink           In-build (route 1): runs inside MSBuild after BuildClassesDex. Builds the DEX
                   with R8 from the original JARs, generates keep rules automatically (JNI names
                   from the .so, manifest entry points, embedded JAR rules, AAR proguard.txt) and
                   replaces the DEX files in $(ClassesDir).
  bundle-metadata  Adds mapping.txt and r8.json to BUNDLE-METADATA of the UNSIGNED AAB.
  post-aab         Route 2 (emergency only, NOT end-to-end tested): rewrite a finished AAB. Needs the
                   list of original JARs (e.g. inputs.txt from route 1) and, for AAR consumer rules,
                   --consumer-rules. Re-sign afterwards.
  check-dex        Pre-flight on any .aab/.apk: DEX size and required classes (FMXNativeActivity).
  verify           Checks that every class named via JNI in the .so is present under its original
                   name in the DEX. Necessary but NOT sufficient: a device/emulator start test is
                   mandatory after every R8 build.

Standard library only. Python 3.10+. Requires R8 >= 9.0.32 (Delphi's bundled R8 8.6.17 rejects
--build-metadata-output).
"""
from __future__ import annotations
import argparse, io, os, re, shutil, struct, subprocess, sys, zipfile
from pathlib import Path

# ---------------------------------------------------------------- read class files

def parse_class(data: bytes):
    """Returns (name, super, [interfaces]) in slash format, or None."""
    if data[:4] != b"\xca\xfe\xba\xbe":
        return None
    pos = 8
    (count,) = struct.unpack_from(">H", data, pos); pos += 2
    utf8, cls = {}, {}
    i = 1
    while i < count:
        tag = data[pos]; pos += 1
        if tag == 1:
            (ln,) = struct.unpack_from(">H", data, pos); pos += 2
            utf8[i] = data[pos:pos + ln].decode("utf-8", "replace"); pos += ln
        elif tag == 7:
            (cls[i],) = struct.unpack_from(">H", data, pos); pos += 2
        elif tag in (8, 16, 19, 20):
            pos += 2
        elif tag == 15:
            pos += 3
        elif tag in (3, 4, 9, 10, 11, 12, 17, 18):
            pos += 4
        elif tag in (5, 6):
            pos += 8; i += 1
        else:
            return None
        i += 1
    _access, this_i, super_i, n_if = struct.unpack_from(">HHHH", data, pos); pos += 8
    ifs = [struct.unpack_from(">H", data, pos + 2 * k)[0] for k in range(n_if)]
    name = lambda ci: utf8.get(cls.get(ci, 0)) if ci else None
    return name(this_i), name(super_i), [name(x) for x in ifs]

def index_jars(jars: list[Path]):
    """Class hierarchy of all program JARs plus their embedded keep rules."""
    hier: dict[str, tuple[str | None, list[str]]] = {}
    embedded: list[tuple[str, str]] = []
    for jar in jars:
        with zipfile.ZipFile(jar) as z:
            for n in z.namelist():
                if n.endswith(".class") and not n.startswith("META-INF/"):
                    r = parse_class(z.read(n))
                    if r and r[0]:
                        hier[r[0]] = (r[1], r[2])
                elif n.startswith(("META-INF/proguard/", "META-INF/com.android.tools/")) and n.endswith((".pro", ".txt")):
                    # R8-specific subfolders (r8-from-x, r8-upto-y) deliberately simplified: take all
                    if "/proguard/" in n or "/r8" in n:
                        embedded.append((f"{jar.name}!{n}", z.read(n).decode("utf-8", "replace")))
    return hier, embedded

# ---------------------------------------------------------------- JNI names from the .so

JNI_RE = re.compile(rb"L?([a-z][a-z0-9_]*(?:/[A-Za-z0-9_$]+){2,});?")

def scan_native(sos: list[Path], program: set[str]) -> set[str]:
    found: set[str] = set()
    for so in sos:
        data = so.read_bytes()
        for m in re.finditer(rb"[\x20-\x7e]{6,}", data):
            for j in JNI_RE.finditer(m.group(0)):
                n = j.group(1).decode()
                if n in program:
                    found.add(n)
                # dotted notation (JavaClass names in strings, Class.forName)
    for so in sos:
        for m in re.finditer(rb"[a-z][a-z0-9_]*(?:\.[A-Za-z0-9_$]+){2,}", so.read_bytes()):
            n = m.group(0).decode().replace(".", "/")
            if n in program:
                found.add(n)
    return found

def scan_manifest(manifest: Path, program: set[str]) -> set[str]:
    if not manifest or not manifest.exists():
        return set()
    text = manifest.read_text("utf-8", "replace")
    pkg = (re.search(r'package="([^"]+)"', text) or [None, ""])[1]
    found = set()
    for v in re.findall(r'"([A-Za-z_][A-Za-z0-9_$.]*)"', text):
        cand = (pkg + v) if v.startswith(".") else v
        cand = cand.replace(".", "/")
        if cand in program:
            found.add(cand)
    # Firebase components appear as meta-data keys "com.google.firebase.components:<class>"
    for v in re.findall(r'components:([A-Za-z0-9_$.]+)"', text):
        cand = v.replace(".", "/")
        if cand in program:
            found.add(cand)
    return found

def closure(names: set[str], hier) -> set[str]:
    """Add super classes and interfaces: Delphi calls inherited methods through the subclass name."""
    out, todo = set(), list(names)
    while todo:
        n = todo.pop()
        if n in out or n not in hier:
            continue
        out.add(n)
        sup, ifs = hier[n]
        todo.extend([x for x in [sup, *ifs] if x])
    return out

def dotted(n: str) -> str:
    return n.replace("/", ".")

# ---------------------------------------------------------------- commands

def read_list(p: str | None) -> list[Path]:
    if not p or not Path(p).exists():
        return []
    return [Path(l.strip()) for l in Path(p).read_text("utf-8-sig").splitlines() if l.strip()]

def android_jar(sdk_api_path: str) -> Path:
    p = Path(sdk_api_path)
    for c in (p / "android.jar", p):
        if c.is_file():
            return c
    raise SystemExit(f"DX.R8: android.jar not found under {sdk_api_path}")

def cmd_shrink(a):
    work = Path(a.work); work.mkdir(parents=True, exist_ok=True)
    jars = [j for j in read_list(a.inputs) if j.suffix == ".jar" and j.exists()]
    missing = [j for j in read_list(a.inputs) if not j.exists()]
    if missing:
        raise SystemExit("DX.R8: input JAR missing: " + ", ".join(map(str, missing)))
    hier, embedded = index_jars(jars)
    program = set(hier)
    sos = read_list(a.natives)
    jni = scan_native([s for s in sos if s.exists()], program)
    man = scan_manifest(Path(a.manifest) if a.manifest else None, program)
    keep_full = closure(jni, hier)          # keep class + all members
    keep_entry = man - keep_full            # entry points: class + constructors

    rules = ["# generated by dx_r8.py, do not edit",
             f"# {len(jni)} classes from the .so, {len(keep_full)} with hierarchy, {len(keep_entry)} from the manifest"]
    rules += [f"-keep class {dotted(n)} {{ *; }}" for n in sorted(keep_full)]
    rules += [f"-keep class {dotted(n)} {{ <init>(...); }}" for n in sorted(keep_entry)]
    gen = work / "generated-rules.pro"; gen.write_text("\n".join(rules) + "\n", "utf-8")

    confs = [Path(a.base_rules), gen]
    for i, (src, txt) in enumerate(embedded):
        f = work / f"embedded-{i:03d}.pro"; f.write_text(f"# from {src}\n{txt}", "utf-8"); confs.append(f)
    # Consumer rules: when Delphi extracts an AAR it keeps only classes.jar, res.zip and the manifest;
    # proguard.txt is dropped. That is why the list also contains the .aar files themselves
    # (DX.R8.targets passes @(AarReference)). Without this, AdMob/UMP builds crash at runtime.
    n_cons = 0
    for c in read_list(a.consumer_rules):
        if not c.exists():
            continue
        if c.suffix.lower() == ".aar":
            with zipfile.ZipFile(c) as z:
                if "proguard.txt" in z.namelist():
                    f = work / f"consumer-{n_cons:03d}.pro"; n_cons += 1
                    f.write_text(f"# from {c.name}!proguard.txt\n" + z.read("proguard.txt").decode("utf-8", "replace"), "utf-8")
                    confs.append(f)
        else:
            confs.append(c); n_cons += 1
    if a.extra_rules and Path(a.extra_rules).exists():
        confs.append(Path(a.extra_rules))

    out = work / "dex"; shutil.rmtree(out, ignore_errors=True); out.mkdir()
    cmd = [a.java, "-Xmx4g", "-cp", a.r8, "com.android.tools.r8.R8", "--release",
           "--min-api", str(a.min_api), "--lib", str(android_jar(a.sdk_api_path)),
           "--output", str(out),
           "--pg-map-output", str(work / "mapping.txt"),
           "--pg-conf-output", str(work / "configuration.txt"),
           "--build-metadata-output", str(work / "r8.json")]
    for c in confs:
        cmd += ["--pg-conf", str(c)]
    cmd += [str(j) for j in jars]
    rsp = work / "r8.rsp"   # Windows command line is too short for 80+ JARs
    # java @argfile: inside quotes "\\" is an escape, hence forward slashes
    rsp.write_text("\n".join('"' + x.replace("\\", "/") + '"' for x in cmd[4:]), "utf-8")
    r = subprocess.run(cmd[:4] + [f"@{rsp}"], capture_output=True, text=True)
    (work / "r8.log").write_text(r.stdout + r.stderr, "utf-8")
    if r.returncode != 0:
        sys.stderr.write(r.stdout + r.stderr)
        if "build-metadata-output" in (r.stdout + r.stderr):
            sys.stderr.write("DX.R8: this R8 does not support --build-metadata-output; use R8 9.0.32 or newer "
                             "(https://storage.googleapis.com/r8-releases/raw/9.0.32/r8.jar), not Delphi's bundled 8.6.17\n")
        raise SystemExit(f"DX.R8: R8 failed (exit {r.returncode}), log: {work / 'r8.log'}")
    n_missing = len(set(re.findall(r"Missing class ([\w.$]+)", r.stdout + r.stderr)))
    if n_missing:
        print(f"DX.R8: WARNING {n_missing} missing classes hidden by -ignorewarnings, see {work / 'r8.log'}; "
              "review them and replace -ignorewarnings with targeted -dontwarn rules")

    dexes = sorted(out.glob("classes*.dex"))
    if not dexes:
        raise SystemExit("DX.R8: R8 produced no DEX")
    cdir = Path(a.classes_dir)
    for old in cdir.glob("classes*.dex"):
        old.unlink()
    for d in dexes:
        shutil.copy2(d, cdir / d.name)
    (work / "jni-classes.txt").write_text("\n".join(sorted(jni)) + "\n", "utf-8")
    print(f"DX.R8: {len(jars)} JARs, {len(jni)} JNI classes, {len(embedded)} embedded and {n_cons} consumer rule files, {len(dexes)} DEX file(s) -> {cdir}")

def cmd_bundle_metadata(a):
    work, aab = Path(a.work), Path(a.aab)
    entries = {"BUNDLE-METADATA/com.android.tools.build.obfuscation/proguard.map": work / "mapping.txt",
               "BUNDLE-METADATA/com.android.tools/r8.json": work / "r8.json"}
    with zipfile.ZipFile(aab) as z:
        if any(n.startswith("META-INF/") and n.endswith((".SF", ".RSA", ".EC")) for n in z.namelist()):
            raise SystemExit("DX.R8: AAB is already signed; add metadata only before signing")
        present = set(z.namelist())
    with zipfile.ZipFile(aab, "a", zipfile.ZIP_DEFLATED) as z:
        for name, src in entries.items():
            if src.exists() and name not in present:
                z.write(src, name)
    print(f"DX.R8: metadata added to {aab.name}")

def cmd_post_aab(a):
    """Route 2: unpack AAB, rebuild base/dex/ with R8, repack. Caller signs."""
    work = Path(a.work); shutil.rmtree(work, ignore_errors=True); work.mkdir(parents=True)
    src, dst = Path(a.aab), Path(a.out)
    with zipfile.ZipFile(src) as z:
        names = z.namelist()
        for n in names:
            if n.endswith(".so"):
                z.extract(n, work / "so")
        man = work / "AndroidManifest.pb"   # proto manifest: class names are stored as plain text
        man.write_bytes(z.read("base/manifest/AndroidManifest.xml"))
    sos = list((work / "so").rglob("*.so"))
    (work / "natives.txt").write_text("\n".join(map(str, sos)), "utf-8")
    classes = work / "classes"; classes.mkdir()
    a2 = argparse.Namespace(**{**vars(a), "natives": str(work / "natives.txt"), "manifest": str(man),
                                "classes_dir": str(classes)})
    if not a.consumer_rules:
        print("DX.R8: WARNING post-aab without --consumer-rules: AAR proguard.txt rules are NOT applied. "
              "Pass a list file with the .aar paths, or expect runtime crashes (AdMob/UMP).")
    cmd_shrink(a2)
    with zipfile.ZipFile(src) as zi, zipfile.ZipFile(dst, "w", zipfile.ZIP_DEFLATED) as zo:
        for info in zi.infolist():
            n = info.filename
            if n.startswith("base/dex/") or n.startswith("META-INF/"):
                continue   # drop old DEX and old signature
            zo.writestr(info, zi.read(n))
        for d in sorted(classes.glob("classes*.dex")):
            zo.write(d, f"base/dex/{d.name}")
        for name, f in {"BUNDLE-METADATA/com.android.tools.build.obfuscation/proguard.map": work / "mapping.txt",
                        "BUNDLE-METADATA/com.android.tools/r8.json": work / "r8.json"}.items():
            if f.exists() and name not in zi.namelist():
                zo.write(f, name)
    print(f"DX.R8: wrote {dst}, UNSIGNED. Sign it now with jarsigner (check the exit code).")

def dex_class_names(dex: bytes) -> set[str]:
    """Class names from a DEX (string_ids/type_ids/class_defs)."""
    (sid_n, sid_off, tid_n, tid_off) = struct.unpack_from("<IIII", dex, 0x38)
    (cd_n, cd_off) = struct.unpack_from("<II", dex, 0x60)
    def s(i):
        (off,) = struct.unpack_from("<I", dex, sid_off + 4 * i)
        p = off
        while dex[p] & 0x80: p += 1
        p += 1; e = dex.index(b"\0", p)
        return dex[p:e].decode("utf-8", "replace")
    out = set()
    for k in range(cd_n):
        (tidx,) = struct.unpack_from("<I", dex, cd_off + 32 * k)
        (sidx,) = struct.unpack_from("<I", dex, tid_off + 4 * tidx)
        out.add(s(sidx)[1:-1])
    return out

def cmd_verify(a):
    jni = [l.strip() for l in Path(a.jni).read_text().splitlines() if l.strip()]
    names: set[str] = set()
    if a.aab:   # read base/dex/classes*.dex straight from an .aab (or lib/apk: classes*.dex)
        with zipfile.ZipFile(a.aab) as z:
            for n in z.namelist():
                if re.fullmatch(r"(base/dex/)?classes\d*\.dex", n):
                    names |= dex_class_names(z.read(n))
    else:
        for d in Path(a.classes_dir).glob("classes*.dex"):
            names |= dex_class_names(d.read_bytes())
    if not names:
        raise SystemExit("DX.R8: no classes*.dex found")
    miss = [n for n in jni if n not in names]
    print(f"{len(jni) - len(miss)}/{len(jni)} JNI classes present under their original names")
    for n in miss[:50]:
        print("MISSING:", n)
    sys.exit(1 if miss else 0)

def cmd_check_dex(a):
    """Pre-flight: are the classes the app cannot start without in the DEX? Catches a missing
    <EnabledSysJars> (Delphi 13 then packs no system JARs; the DEX is tiny and has no FMXNativeActivity)."""
    names: set[str] = set()
    total = 0
    with zipfile.ZipFile(a.aab) as z:
        for n in z.namelist():
            if re.fullmatch(r"(base/dex/)?classes\d*\.dex", n):
                data = z.read(n); total += len(data); names |= dex_class_names(data)
    req = a.require or ["com/embarcadero/firemonkey/FMXNativeActivity"]
    miss = [r.replace(".", "/") for r in req if r.replace(".", "/") not in names]
    print(f"DEX: {total} bytes ({total / 1e6:.2f} MB decimal, {total / 1048576:.2f} MiB), {len(names)} classes")
    if total > 10 * 1000 * 1000:
        print("NOTE: DEX above 10 MB, Google Play expects R8 optimization for this app")
    for m in miss:
        print("MISSING:", m)
    sys.exit(1 if miss else 0)

def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    for name in ("shrink", "post-aab"):
        p = sub.add_parser(name)
        p.add_argument("--java", required=True); p.add_argument("--r8", required=True)
        p.add_argument("--sdk-api-path", required=True); p.add_argument("--min-api", type=int, default=23)
        p.add_argument("--inputs", required=True); p.add_argument("--base-rules", required=True)
        p.add_argument("--extra-rules"); p.add_argument("--work", required=True)
        if name == "shrink":
            p.add_argument("--natives", required=True); p.add_argument("--manifest")
            p.add_argument("--consumer-rules"); p.add_argument("--classes-dir", required=True)
        else:
            p.add_argument("--aab", required=True); p.add_argument("--out", required=True)
            p.add_argument("--consumer-rules", help="list file: .aar files (proguard.txt is read from them) or .pro files")
    p = sub.add_parser("bundle-metadata"); p.add_argument("--aab", required=True); p.add_argument("--work", required=True)
    p = sub.add_parser("verify"); p.add_argument("--jni", required=True)
    g = p.add_mutually_exclusive_group(required=True)
    g.add_argument("--classes-dir"); g.add_argument("--aab", help=".aab or .apk to read classes*.dex from")
    p = sub.add_parser("check-dex"); p.add_argument("--aab", required=True, help=".aab or .apk")
    p.add_argument("--require", action="append", help="class that must be present (repeatable), default FMXNativeActivity")
    a = ap.parse_args()
    {"shrink": cmd_shrink, "post-aab": cmd_post_aab, "bundle-metadata": cmd_bundle_metadata, "verify": cmd_verify,
     "check-dex": cmd_check_dex}[a.cmd](a)

if __name__ == "__main__":
    main()
