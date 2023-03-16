import com.shaft.driver.SHAFT;
import org.json.simple.JSONObject;
import org.testng.annotations.Test;

public class test {



    @Test
    public void testAPI(){
        SHAFT.API api = new SHAFT.API("http://localhost:1802/api/cart");
        JSONObject body = new JSONObject();
        body.put("BusinessEntityID", 10001);
        body.put("BusinessEntityGUID", "2B70DD02-D874-4C1A-8906-D30C8C44CF53");
        api.get("/getorderrequestdatetime").setRequestBody(body).setContentType("application/json").perform();

    }


}
