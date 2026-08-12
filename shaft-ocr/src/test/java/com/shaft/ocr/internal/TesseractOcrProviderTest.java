package com.shaft.ocr.internal;

import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrResult;
import org.mockito.ArgumentCaptor;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.file.Path;
import java.util.List;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class TesseractOcrProviderTest {
    @Test
    public void resolvesDefaultsProvisionsModelsAndDelegatesToNativeBackend() {
        TessdataModelManager models = mock(TessdataModelManager.class);
        TesseractBackend backend = mock(TesseractBackend.class);
        Path tessdata = Path.of("build", "ocr-models");
        byte[] image = {1, 2, 3};
        OcrResult expected = new OcrResult("Hello مرحبا", List.of());
        when(models.ensureAvailable(List.of("eng", "ara"))).thenReturn(tessdata);
        when(backend.recognize(eq(image), eq(tessdata), eq("eng+ara"), eq(OcrOptions.defaults())))
                .thenReturn(expected);

        TesseractOcrProvider provider = new TesseractOcrProvider(models, backend);

        Assert.assertSame(provider.recognize(image, OcrOptions.defaults()), expected);
        Assert.assertEquals(provider.name(), "tesseract-local");
        Assert.assertEquals(provider.priority(), 100);
    }

    @Test
    public void explicitHumanLanguageNamesReachBackendAsTesseractCodes() {
        TessdataModelManager models = mock(TessdataModelManager.class);
        TesseractBackend backend = mock(TesseractBackend.class);
        Path tessdata = Path.of("build", "ocr-models");
        when(models.ensureAvailable(List.of("fra", "deu"))).thenReturn(tessdata);
        when(backend.recognize(eq(new byte[]{9}), eq(tessdata), eq("fra+deu"), org.mockito.ArgumentMatchers.any()))
                .thenReturn(new OcrResult("bonjour", List.of()));

        OcrOptions options = OcrOptions.defaults().withLanguages("French", "German");
        new TesseractOcrProvider(models, backend).recognize(new byte[]{9}, options);

        ArgumentCaptor<OcrOptions> optionsCaptor = ArgumentCaptor.forClass(OcrOptions.class);
        verify(backend).recognize(eq(new byte[]{9}), eq(tessdata), eq("fra+deu"), optionsCaptor.capture());
        Assert.assertEquals(optionsCaptor.getValue(), options);
    }
}
