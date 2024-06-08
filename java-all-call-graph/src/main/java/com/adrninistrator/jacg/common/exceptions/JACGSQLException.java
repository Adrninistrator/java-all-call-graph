package com.adrninistrator.jacg.common.exceptions;

/**
 * @author adrninistrator
 * @date 2024/4/25
 * @description:
 */
public class JACGSQLException extends RuntimeException {

    public JACGSQLException(String message) {
        super(message, null, true, false);
    }
}
