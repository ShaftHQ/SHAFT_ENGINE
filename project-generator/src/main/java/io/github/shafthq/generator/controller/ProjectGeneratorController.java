package io.github.shafthq.generator.controller;

import io.github.shafthq.generator.model.ProjectConfiguration;
import io.github.shafthq.generator.service.ProjectGeneratorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

@Controller
public class ProjectGeneratorController {

    @Autowired
    private ProjectGeneratorService generatorService;

    @GetMapping("/")
    public String index() {
        return "index";
    }

    @PostMapping("/generate")
    @ResponseBody
    public ResponseEntity<byte[]> generateProject(@RequestBody ProjectConfiguration config) {
        try {
            byte[] zipContent = generatorService.generateProject(config);

            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_OCTET_STREAM);
            headers.setContentDispositionFormData("attachment", config.getArtifactId() + ".zip");
            headers.setContentLength(zipContent.length);

            return new ResponseEntity<>(zipContent, headers, HttpStatus.OK);
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }
}
