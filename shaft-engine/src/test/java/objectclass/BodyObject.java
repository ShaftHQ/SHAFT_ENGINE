package objectclass;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class BodyObject {
    @JsonProperty("key1")
    private String key1 = null;

    @JsonProperty("key2")
    private String key2 = null;

    @JsonProperty("key3")
    private String key3 = null;

    @JsonProperty("key4")
    private int key4;
}
