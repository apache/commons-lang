import org.apache.commons.lang3.builder.ToStringStyle;


/**
 * 
 * @author anil.bharadia@gmail.com
 *
 */
public class JsonToStringStyle extends ToStringStyle {

	private static final long serialVersionUID = -282943258484693764L;
	
	private String fieldNamePrefix;
	private String fieldNameSuffix;
	
	
	

	public JsonToStringStyle() {
		setUseClassName(false);
		setUseIdentityHashCode(false);
		
		setContentStart("{");
		setContentEnd("}");
		
		setArrayStart("[");
		setArrayEnd("]");
		
		setFieldSeparator(",");
		setFieldNameValueSeparator(":");
		
		setNullText("null");
		
		setFieldNamePrefix("\"");
		setFieldNameSuffix("\"");
		
	}
	
	
	public String getFieldNamePrefix() {
		return fieldNamePrefix;
	}
	public void setFieldNamePrefix(String fieldNamePrefix) {
		if (fieldNamePrefix == null) {
			fieldNamePrefix = "";
		}
		this.fieldNamePrefix = fieldNamePrefix;
	}

	
	public String getFieldNameSuffix() {
		return fieldNameSuffix;
	}
	public void setFieldNameSuffix(String fieldNameSuffix) {
		if (fieldNameSuffix == null) {
			fieldNameSuffix = "";
		}
		this.fieldNameSuffix = fieldNameSuffix;
	}
	
	
	private String decorateFieldName(String fieldName) {
		return getFieldNamePrefix() + fieldName + getFieldNameSuffix();
	}
	
	
	
	@Override
	protected void appendFieldStart(StringBuffer buffer, String fieldName) {
		super.appendFieldStart(buffer, decorateFieldName(fieldName));
	}
	
	
	@Override
	protected void appendDetail(StringBuffer buffer, String fieldName, Object value) {
		
		if (value == null ) {
			appendNullText(buffer, fieldName);
			return;
		}
		
		if (value.getClass() == String.class) {
			appendValueAsString(buffer, (String)value);
			return;
		}
        buffer.append(value);
    }


	private void appendValueAsString(StringBuffer buffer, String value) {
		buffer.append("\"" + value + "\"");
	}
	

}
