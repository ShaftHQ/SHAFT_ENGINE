package fixture;

import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class OpenCvVisualConsumer {
    public Mat resize(Mat source) {
        Mat target = new Mat();
        Imgproc.resize(source, target, new Size(320, 240));
        return target;
    }
}
