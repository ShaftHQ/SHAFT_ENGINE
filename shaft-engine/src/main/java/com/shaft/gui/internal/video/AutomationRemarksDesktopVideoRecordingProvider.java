package com.shaft.gui.internal.video;

import com.automation.remarks.video.RecorderFactory;
import com.automation.remarks.video.RecordingUtils;
import com.automation.remarks.video.enums.RecorderType;
import com.automation.remarks.video.enums.VideoSaveMode;
import com.automation.remarks.video.recorder.IVideoRecorder;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.log4j.BasicConfigurator;
import ws.schild.jave.Encoder;
import ws.schild.jave.EncoderException;
import ws.schild.jave.MultimediaObject;
import ws.schild.jave.encode.AudioAttributes;
import ws.schild.jave.encode.EncodingAttributes;
import ws.schild.jave.encode.VideoAttributes;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Desktop video provider backed by Automation Remarks and JAVE.
 *
 * <p>This implementation remains in {@code shaft-engine} until the dependency extraction performed by the
 * {@code shaft-video} module work. Keeping all optional-library types in this class allows engine orchestration and
 * listeners to depend only on {@link DesktopVideoRecordingProvider}.</p>
 */
public class AutomationRemarksDesktopVideoRecordingProvider implements DesktopVideoRecordingProvider {
    private final ThreadLocal<IVideoRecorder> recorder = new ThreadLocal<>();

    /**
     * Starts a Monte desktop recording for the current test thread.
     */
    @Override
    public void startRecording() {
        if (recorder.get() != null) {
            return;
        }

        BasicConfigurator.configure();
        ThreadLocalPropertiesManager.setGlobalProperty("video.save.mode", VideoSaveMode.ALL.name());
        ThreadLocalPropertiesManager.setGlobalProperty("video.folder", "target/video");
        recorder.set(RecorderFactory.getRecorder(RecorderType.MONTE));
        recorder.get().start();
    }

    /**
     * Stops, processes, and MP4-encodes the current thread's desktop recording.
     *
     * @param testPassed whether the current test passed
     * @param recordingName unique name to use when saving the recording
     * @return the encoded recording, or {@code null} when it cannot be read
     */
    @Override
    public InputStream stopRecording(boolean testPassed, String recordingName) {
        IVideoRecorder activeRecorder = recorder.get();
        if (activeRecorder == null) {
            return null;
        }

        try {
            String recordingPath = RecordingUtils.doVideoProcessing(testPassed,
                    activeRecorder.stopAndSave(recordingName));
            return new FileInputStream(encodeRecording(recordingPath));
        } catch (IOException exception) {
            ReportManagerHelper.logDiscrete(exception);
            return null;
        } finally {
            recorder.remove();
        }
    }

    /**
     * Reports whether a desktop recording is active for the current test thread.
     *
     * @return {@code true} when recording; otherwise {@code false}
     */
    @Override
    public boolean isRecording() {
        return recorder.get() != null;
    }

    @SuppressWarnings("SpellCheckingInspection")
    private File encodeRecording(String recordingPath) {
        File source = new File(recordingPath);
        File target = new File(recordingPath.replace("avi", "mp4"));
        try {
            AudioAttributes audio = new AudioAttributes();
            audio.setCodec("libvorbis");
            VideoAttributes video = new VideoAttributes();
            EncodingAttributes attributes = new EncodingAttributes();
            attributes.setOutputFormat("mp4");
            attributes.setAudioAttributes(audio);
            attributes.setVideoAttributes(video);
            new Encoder().encode(new MultimediaObject(source), target, attributes);
        } catch (EncoderException exception) {
            ReportManagerHelper.logDiscrete(exception);
        }
        return target;
    }
}
