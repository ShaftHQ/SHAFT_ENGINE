# shaft-sikulix uses OculiX 4.0.0 (not archived sikulixapi 2.0.5)

`com.sikulix:sikulixapi:2.0.5` is archived. Module depends on
`io.github.oculix-org:oculixapi:4.0.0`. OpenCV/Tesseract natives come from Apertix/Legerix;
do not reintroduce `SikuliNativeLibraryStager` or pin `org.openpnp:opencv` in this module
(two OpenCVs on the classpath break loading). Public API stays `org.sikuli.script.*` by
design. `App.waitForWindow(int)` is gone in OculiX — use `App.isRunning(int)` before
`focus()`. See #6160.
