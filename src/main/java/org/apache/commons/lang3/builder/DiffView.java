package org.apache.commons.lang3.builder;

/**
 * <p>A {@code DiffView} object holds the string representation of the difference of
 * a given field between two objects.</p>
 *
 * @since 3.10
 */
public class DiffView {
	private final String field;
	private final String leftValue;
	private final String rightValue;

	/**
	 * <p>Constructs a {@code DiffView} from 3 string params: field, leftValue and rightValue</p>
	 *
	 * @param field designates the field identified as different.
	 * @param leftValue holds the string representation of the value in the left side of the comparison.
	 * @param rightValue holds the string representation of the value in the right side of the comparison.
	 */
	public DiffView(String field, String leftValue, String rightValue) {
		this.field = field;
		this.leftValue = leftValue;
		this.rightValue = rightValue;
	}

	/**
	 * <p>Constructs a {@code DiffView} from a {@link Diff} object, by formatting to string
	 * its field name, and left and right values.</p>
	 *
	 * @param diff the {@link Diff} object from which the field name, and the left & right value will be extracted.
	 */
	public DiffView(Diff diff) {
		this.field = diff.getFieldName();
		this.leftValue =  String.format("%s", diff.getLeft());
		this.rightValue = String.format("%s", diff.getRight());
	}

	/**
	 * <p>Returns the field.</p>
	 *
	 * @return the field.
	 */
	public String getField() {
		return field;
	}

	/**
	 * <p>Returns the left value.</p>
	 * @return the left value.
	 */
	public String getLeftValue() {
		return leftValue;
	}

	/**
	 * <p>Returns the right value.</p>
	 * @return the right value.
	 */
	public String getRightValue() {
		return rightValue;
	}

}
