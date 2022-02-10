package com.shaft.awsObjects;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.ListObjectsV2Result;
import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.S3ObjectInputStream;
import com.amazonaws.services.s3.model.S3ObjectSummary;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;

public class S3 {
    String bucketName ;

    public S3(String bucketName) {
        this.bucketName = bucketName;
    }

    final static AmazonS3 s3 = AmazonS3ClientBuilder.standard()
            .withRegion(System.getProperty("awsRegion")).build();
    public  List<S3ObjectSummary> getList()
    {
        System.out.format("Objects in S3 bucket %s:\n", bucketName);
        ListObjectsV2Result result = s3.listObjectsV2(bucketName);
        List<S3ObjectSummary> objects = result.getObjectSummaries();
        for (S3ObjectSummary os : objects) {
            System.out.println("* " + os.getKey());
        }
        return objects;
    }
    public void upload(String filePath,String fileNameOnS3){
        System.out.format("Uploading %s to S3 bucket %s...\n", filePath, bucketName);
        try {
            s3.putObject(bucketName, fileNameOnS3, new File(filePath));
        } catch (AmazonServiceException e) {
            System.err.println(e.getErrorMessage());
            System.exit(1);
        }
    }
   public void download(String fileNameOnS3)
    {
        System.out.format("Downloading %s from S3 bucket %s...\n", fileNameOnS3, bucketName);
        try {
            S3Object o = s3.getObject(bucketName, fileNameOnS3);
            S3ObjectInputStream s3is = o.getObjectContent();
            FileOutputStream fos = new FileOutputStream(new File(fileNameOnS3));
            byte[] read_buf = new byte[1024];
            int read_len = 0;
            while ((read_len = s3is.read(read_buf)) > 0) {
                fos.write(read_buf, 0, read_len);
            }
            s3is.close();
            fos.close();
        } catch (AmazonServiceException e) {
            System.err.println(e.getErrorMessage());
            System.exit(1);
        } catch (FileNotFoundException e) {
            System.err.println(e.getMessage());
            System.exit(1);
        } catch (IOException e) {
            System.err.println(e.getMessage());
            System.exit(1);}
    }
    public void delete(String fileNameOnS3)
    {
        try {
            s3.deleteObject(bucketName,fileNameOnS3);
        } catch (AmazonServiceException e) {
            System.err.println(e.getErrorMessage());
            System.exit(1);
        }
    }
}
