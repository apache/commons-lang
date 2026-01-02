package com.demo.web;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class HelloController {

    @GetMapping("/")
    public String home() {
        return "Demo App is running!";
    }

    @GetMapping("/hello")
    public String hello() {
        return "Hello, Demo App!";
    }
}
