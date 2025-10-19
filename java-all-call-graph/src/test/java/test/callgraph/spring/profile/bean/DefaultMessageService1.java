package test.callgraph.spring.profile.bean;

/**
 * @author adrninistrator
 * @date 2025/10/18
 * @description:
 */
public class DefaultMessageService1 implements MessageService1 {
    private String message;

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public String getMessage() {
        return message;
    }
}