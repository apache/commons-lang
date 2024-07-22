package org.apache.commons.lang3.builder;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * A {@code DiffResultView} encapsulates two objects of the same type and list their differences
 * as a list of {@link DiffView} objects, each holding a field name and the two different values found for this field
 * in the Left and Right object.
 * </p>
 *
 * <p>
 * It can be built with or without a pre-defined list of {@link DiffView} objects, or from an existing
 * {@link DiffResult}.
 * </p>
 *
 * @param <T> type of the left and right object.
 *
 * @since 3.10
 */
public class DiffResultView<T> {

	/**
	 * The list of {@link DiffView} objects for the two compared objects.
	 */
	private List<DiffView> diffs;

	/**
	 * Holds the left object of the comparison.
	 */
	private T left;
	/**
	 * Holds the right object of the comparison.
	 */
	private T right;

	/**
	 * <p>Creates a simple {@code DiffResultView} without predefined list of {@link DiffView}.
	 * The list will be initialized as an empty @{link ArrayList}.</p>
	 *
	 * @param o1 left object of the comparison
	 * @param o2 right object of the comparison
	 */
	public DiffResultView(T o1, T o2) {
		this.left = o1;
		this.right = o2;
		this.diffs = new ArrayList<>();
	}

	/**
	 * <p>Creates a simple {@code DiffResultView} with a predefined list of {@link DiffView}.</p>
	 *
	 * @param o1 left object of the comparison.
	 * @param o2 right object of the comparison.
	 * @param diffs list of {@link DiffView} objects.
	 */
	public DiffResultView(T o1, T o2, List<DiffView> diffs) {
		this(o1, o2);
		this.diffs = diffs;
	}

	/**
	 * <p>Creates a simple {@code DiffResultView} from a {@link DiffResult} object.</p>
	 * <p>{@link DiffView} objects will be constructed from {@link DiffResult} {@link Diff} objects and
	 * added to the diffs list</p>
	 *
	 * @param diffResult {@link DiffResult} object used to initialize the {@code DiffResultView}
	 */
	public DiffResultView(DiffResult<T> diffResult) {
		this(diffResult.getLeft(), diffResult.getRight());
		this.addDiffs(diffResult);
	}

	/**
	 * <p>Returns the list of the {@link DiffView} objects</p>
	 * @return the list of the {@link DiffView} objects
	 */
	public List<DiffView> getDiffs() {
		return diffs;
	}

	/**
	 * <p>Returns only a list of the fields of all {@link DiffView} objects, that is, fields that have been detected
	 * as changed between the objects.</p>
	 *
	 * @return the list of the {@link DiffView} objects
	 */
	public List<String> getDiffFields() {
		return diffs.stream().map(DiffView::getField).collect(Collectors.toList());
	}

	/**
	 * <p>Adds one {@link DiffView} to the list of the diffs between the two objects.</p>
	 *
	 * @param diffView the {@link DiffView} object to be added to the diffs list.
	 */
	public void addDiff(DiffView diffView) {
		diffs.add(diffView);
	}

	/**
	 * <p>Builds a {@link DiffView} from a {@link Diff} object and adds it to the diffs list.</p>
	 *
	 * @param diff the {@link Diff} object from which the {@link DiffView} object will be built and added to the list.
	 */
	public void addDiff(Diff<?> diff) {
		diffs.add(new DiffView(diff));
	}

	/**
	 * <p>Builds a {@link DiffView} from each {@link Diff} object of the {@link DiffResult}
	 * and adds them to the diffs list.</p>
	 *
	 * @param diffResult the {@link DiffResult} object from which the {@link DiffView} objects
	 *                      will be built and added to the list.
	 */
	public void addDiffs(DiffResult<?> diffResult) {
		diffs.addAll(
				diffResult
						.getDiffs()
						.stream()
						.map(DiffView::new)
						.collect(Collectors.toList())
		);
	}

	/**
	 *
	 * <p>Adds all {@link DiffView} objects from another {@link DiffResultView} to the diffs list.</p>
	 *
	 * @param diffResultView the {@link DiffResultView} object from which the {@link DiffView} objects
	 *                          will be added to the list.
	 */
	public void addDiffs(DiffResultView<?> diffResultView) {
		diffs.addAll(diffResultView.getDiffs());
	}

	/**
	 * <p>Returns the left object of this {@code DiffResultView}.</p>
	 *
	 * @return the left object of this {@code DiffResultView}.
	 */
	public T getLeft() {
		return this.left;
	}

	/**
	 * <p>Returns the right object of this {@code DiffResultView}.</p>
	 *
	 * @return the right object of this {@code DiffResultView}.
	 */
	public T getRight() {
		return this.right;
	}
}
