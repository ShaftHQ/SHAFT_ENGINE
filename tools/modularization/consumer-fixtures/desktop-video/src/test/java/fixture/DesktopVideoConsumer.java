package fixture;

import com.automation.remarks.video.RecorderFactory;
import com.automation.remarks.video.enums.RecorderType;
import com.automation.remarks.video.recorder.IVideoRecorder;
import ws.schild.jave.Encoder;

public class DesktopVideoConsumer {
    public IVideoRecorder recorder() {
        Encoder.class.getName();
        return RecorderFactory.getRecorder(RecorderType.MONTE);
    }
}
