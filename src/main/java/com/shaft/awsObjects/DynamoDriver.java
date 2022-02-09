package com.shaft.awsObjects;



import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.spec.DeleteItemSpec;
import com.amazonaws.services.dynamodbv2.document.spec.GetItemSpec;

public class DynamoDriver {

    private AmazonDynamoDB client=AmazonDynamoDBClientBuilder.standard()
            .withRegion(System.getProperty("awsRegion"))
                .build();
    private DynamoDB dynamoDB= new DynamoDB(client);

    public Item getItem(String tableName, GetItemSpec getItemSpec, String selectionConditionMsg) {
        Table table = dynamoDB.getTable(tableName);
        try {
            System.out.println("Attempting to read the item from "+ tableName + " with "+selectionConditionMsg);
            Item bill = table.getItem(getItemSpec);
            System.out.println("GetItem succeeded: " + bill.toJSONPretty());
            return bill;
        } catch (Exception e) {
            System.err.println(e.getMessage());
        }
        return null;
    }
    public void deleteItem(String tableName, DeleteItemSpec deleteItemSpec, String deletionConditionMsg) {
        Table table = dynamoDB.getTable(tableName);
        try {
            System.out.println("Attempting a conditional delete...");
            table.deleteItem(deleteItemSpec);
            System.out.println("DeleteItem succeeded");
        } catch (Exception e) {
            System.err.println("Unable to delete item: " + deletionConditionMsg);
            System.err.println(e.getMessage());
        }
    }
}