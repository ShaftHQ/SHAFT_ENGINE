package objectclass;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class BodyObject {
    @JsonProperty("full_name")
    private String full_name = null;

    @JsonProperty("phone")
    private String phone = null;

    @JsonProperty("birthday")
    private String birthday = null;

    @JsonProperty("governorate_id")
    private int governorate_id;

    @JsonProperty("city_id")
    private int city_id;

    @JsonProperty("country_id")
    private int country_id;

    @JsonProperty("email")
    private String email = null;

    @JsonProperty("password")
    private String password = null;

    @JsonProperty("passwordConfirm")
    private String passwordConfirm = null;

    @JsonProperty("type")
    private String type = null;

    @JsonProperty("gender")
    private String gender = null;

    @JsonProperty("id_number")
    private String id_number = null;
}
