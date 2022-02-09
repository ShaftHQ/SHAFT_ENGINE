package com.shaft.awsObjects;

import lombok.SneakyThrows;
import software.amazon.awssdk.auth.credentials.ProfileCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.sqs.SqsClient;
import software.amazon.awssdk.services.sqs.model.*;

import java.net.URI;
import java.util.List;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClientBuilder;
public class SQS {
    SqsClient sqsClient;
    String queueUrl;


    public SQS(String queueName)
    {
        AmazonSQS sqs = AmazonSQSClientBuilder.defaultClient();
        this.queueUrl= sqs.getQueueUrl(queueName).getQueueUrl();
    }
    @SneakyThrows
    public void open()
    {
        this.sqsClient = SqsClient.builder()
                .region(Region.of(System.getProperty("awsRegion")))
                .endpointOverride(new URI(this.queueUrl))
                .credentialsProvider(ProfileCredentialsProvider.create())
                .build();
//        System.out.println(queueUrl);
    }
    public String sendMsg(String MsgContent, int delayInSeconds)
    {open();
     SendMessageResponse value= sqsClient.sendMessage(SendMessageRequest.builder()
                .queueUrl(queueUrl)
                .messageBody(MsgContent)
                .delaySeconds(delayInSeconds)
                .build());
        close();
       return value.messageId();
    }
    public List<Message> receiveMsgs(int NumOfMsgs){
        open();
        try{
        ReceiveMessageRequest receiveMessageRequest = ReceiveMessageRequest.builder()
                .queueUrl(queueUrl)
                .maxNumberOfMessages(NumOfMsgs)
                .build();
        List<Message> messages = sqsClient.receiveMessage(receiveMessageRequest).messages();
//            System.out.println( messages.size());
            return messages;
    } catch (SqsException e) {
        System.err.println(e.awsErrorDetails().errorMessage());
        System.exit(1);
    }
        finally {
            close();
        }
        return null;

    }
    public boolean findMsg(String MsgId)
    {
        List<Message> msgList= receiveMsgs(10);
        for (Message msg : msgList) {
            if (msg.messageId().equals(MsgId))
                return true;
        }
        return false;
    }
    public void listQueues() {
        open();
        System.out.println("\nList Queues");
        String prefix = "bp";

        try {
            ListQueuesRequest listQueuesRequest = ListQueuesRequest.builder().queueNamePrefix(prefix).build();
            ListQueuesResponse listQueuesResponse = sqsClient.listQueues(listQueuesRequest);

            for (String url : listQueuesResponse.queueUrls()) {
                System.out.println((url));
            }

        } catch (SqsException e) {
            System.err.println(e.awsErrorDetails().errorMessage());
            System.exit(1);
        }
        finally {
            close();
        }
    }
    public void close()
    {
        sqsClient.close();
    }
}
