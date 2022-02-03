package testPackage01;

import com.shaft.api.RestActions;
import com.shaft.validation.Assertions;
import io.restassured.response.Response;
import org.testng.annotations.Test;

public class Test_sendGraphqlRequest {

    String spacex_base_uri = "https://api.spacex.land/";
    @Test
    public void sendGraphqlRequestUsingQuery(){
        String query = "{company {ceo}}";
        Response response = RestActions.sendGraphQlRequest(spacex_base_uri,query);
        Assertions.assertApiResponseEquals(response,"Elon Musk","data.company.ceo");
    }
    @Test
    public void sendGraphqlRequestUsingMutationAndVariables(){
        String mutation = """
                mutation ($name: String, $rocket: String) {
                  insert_users(objects: {name: $name, rocket: $rocket}) {
                    returning {
                      name
                      rocket
                    }
                  }
                }""";
        String variables = """
                {"name": "sherlock holmes","rocket": "221B Baker Street"}""";
        Response response = RestActions.sendGraphQlRequest(spacex_base_uri,mutation,variables);
        Assertions.assertApiResponseEquals(response,"sherlock holmes","data.insert_users.returning[0].name");
    }
}
