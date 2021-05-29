package testPackage01;

import com.shaft.api.RestActions;
import com.shaft.validation.Assertions;
import io.restassured.response.Response;
import org.testng.annotations.Test;

public class Test_sendGraphqlRequest {
    @Test
    public void sendGraphqlRequestUsingQuery(){

        String query = "{company {ceo}}";
        Response response = RestActions.sendGraphqlRequest("https://api.spacex.land/",query);
        Assertions.assertApiResponseEquals(response,"Elon Musk","data.company.ceo");
    }
    @Test
    public void sendGraphqlRequestUsingMutationAndVariables(){

        String mutation = "mutation ($name:String,$rocket:String){\n" +
                "  insert_users(objects: {name: $name, rocket: $rocket}) {\n" +
                "    returning {\n" +
                "      name\n" +
                "      rocket\n" +
                "    }\n" +
                "  }\n" +
                "}\n";
        String variables = "{\"name\": \"test\",\"rocket\": \"test\"}";
        Response response = RestActions.sendGraphqlRequest("https://api.spacex.land/",mutation,variables);

        Assertions.assertApiResponseEquals(response,"test","data.insert_users.returning[0].name");
    }
}
