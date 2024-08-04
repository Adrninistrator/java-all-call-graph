package com.adrninistrator.jacg.spring.configuration;

import org.neo4j.ogm.config.ClasspathConfigurationSource;
import org.neo4j.ogm.config.ConfigurationSource;
import org.neo4j.ogm.session.SessionFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.neo4j.repository.config.EnableNeo4jRepositories;
import org.springframework.data.neo4j.transaction.Neo4jTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * @author adrninistrator
 * @date 2024/7/21
 * @description:
 */
@Configuration
@EnableNeo4jRepositories(
        basePackages = "com.adrninistrator.jacg.neo4j.repository",
        sessionFactoryRef = JACGSpringConfiguration.NEO4J_SESSION_FACTORY_NAME,
        transactionManagerRef = JACGSpringConfiguration.NEO4J_TRANSACTION_MANAGER_NAME
)
public class JACGSpringConfiguration {

    public static final String NEO4J_SESSION_FACTORY_NAME = "jacgNeo4jSessionFactory";
    public static final String NEO4J_TRANSACTION_MANAGER_NAME = "jacgNeo4jTransactionManager";
    public static final String NEO4J_TRANSACTION_TEMPLATE_NAME = "jacgNeo4jTransactionTemplate";

    @Bean(JACGSpringConfiguration.NEO4J_SESSION_FACTORY_NAME)
    public SessionFactory sessionFactory() {
        // with domain entity base package(s)
        return new SessionFactory(configuration(), "com.adrninistrator.jacg.neo4j.domain");
    }

    @Bean
    public org.neo4j.ogm.config.Configuration configuration() {
        ConfigurationSource properties = new ClasspathConfigurationSource("classpath:ogm.properties");
        return new org.neo4j.ogm.config.Configuration.Builder(properties).build();
    }

    @Bean(JACGSpringConfiguration.NEO4J_TRANSACTION_MANAGER_NAME)
    public Neo4jTransactionManager transactionManager() {
        return new Neo4jTransactionManager(sessionFactory());
    }

    @Bean(JACGSpringConfiguration.NEO4J_TRANSACTION_TEMPLATE_NAME)
    public TransactionTemplate transactionTemplate() {
        return new TransactionTemplate(transactionManager());
    }
}