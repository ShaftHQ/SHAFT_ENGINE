package fixture;

import com.shaft.gui.internal.image.VisualProcessingProvider;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import java.util.ServiceLoader;

public class OpenCvVisualConsumer {
    public String automaticallyDiscoveredProvider() {
        return ServiceLoader.load(VisualProcessingProvider.class)
                .findFirst()
                .orElseThrow()
                .getClass()
                .getName();
    }

    public Mat resize(Mat source) {
        Mat target = new Mat();
        Imgproc.resize(source, target, new Size(320, 240));
        return target;
    }

    public static void main(String[] args) {
        String provider = new OpenCvVisualConsumer().automaticallyDiscoveredProvider();
        if (!"com.shaft.gui.internal.image.OpenCvVisualProcessingProvider".equals(provider)) {
            throw new IllegalStateException("Unexpected visual provider: " + provider);
        }
    }

}
