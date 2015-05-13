package visualization;

/**
 * Rectangle Packer v1.3.0
 *
 * Copyright 2012 Ville Koskela. All rights reserved.
 *
 * Email: ville@villekoskela.org
 * Blog: http://villekoskela.org
 * Twitter: @villekoskelaorg
 *
 * You may redistribute, use and/or modify this source code freely
 * but this copyright statement must not be removed from the source files.
 *
 * The package structure of the source code must remain unchanged.
 * Mentioning the author in the binary distributions is highly appreciated.
 *
 * If you use this utility it would be nice to hear about it so feel free to drop
 * an email.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. *
 *
 */

import java.util.ArrayList;

/**
 * Class used to pack rectangles within container rectangle with close to
 * optimal solution.
 */
public class RectanglePacker {

	private double mWidth = 0;
	private double mHeight = 0;
	private int mPadding = 8;

	private double mPackedWidth = 0;
	private double mPackedHeight = 0;

	private ArrayList<SortableSize> mInsertList = new ArrayList<SortableSize>();

	private ArrayList<Rectangle> mInsertedRectangles = new ArrayList<Rectangle>();
	private ArrayList<Rectangle> mFreeAreas = new ArrayList<Rectangle>();
	private ArrayList<Rectangle> mNewFreeAreas = new ArrayList<Rectangle>();

	private Rectangle mOutsideRectangle;

	private ArrayList<SortableSize> mSortableSizeStack = new ArrayList<SortableSize>();
	private ArrayList<Rectangle> mRectangleStack = new ArrayList<Rectangle>();

	public int getRectangleCount() {
		return mInsertedRectangles.size();
	}

	public double getPackedWidth() {
		return mPackedWidth;
	}

	public double getPackedHeight() {
		return mPackedHeight;
	}

	public int getPadding() {
		return mPadding;
	}
	
	public ArrayList<Rectangle> getInsertedRectangle() {
		return mInsertedRectangles;
	}

	/**
	 * Constructs new rectangle packer
	 * 
	 * @param width
	 *            the width of the main rectangle
	 * @param height
	 *            the height of the main rectangle
	 */
	public RectanglePacker(double width, double height, int padding) {
		mOutsideRectangle = new Rectangle(width + 1, height + 1, 0, 0);
		reset(width, height, padding);
	}

	/**
	 * Resets the rectangle packer with given dimensions
	 * 
	 * @param width
	 * @param height
	 */
	public void reset(double width, double height, int padding) {
		while (mInsertedRectangles.size() > 0) {
			int index = mInsertedRectangles.size() - 1;
			freeRectangle(mInsertedRectangles.remove(index));
		}

		while (mFreeAreas.size() > 0) {
			int index = mFreeAreas.size() - 1;
			freeRectangle(mFreeAreas.remove(index));
		}

		mWidth = width;
		mHeight = height;

		mPackedWidth = 0;
		mPackedHeight = 0;

		mFreeAreas.add(0, allocateRectangle(0, 0, mWidth, mHeight));

		while (mInsertList.size() > 0) {
			int index = mInsertList.size() - 1;
			freeSize(mInsertList.remove(index));
		}

		mPadding = padding;
	}

	/**
	 * Gets the position of the rectangle in given index in the main rectangle
	 * 
	 * @param index
	 *            the index of the rectangle
	 * @param rectangle
	 *            an instance where to set the rectangle's values
	 * @return
	 */
	public Rectangle getRectangle(int index, Rectangle rectangle) {
		Rectangle inserted = mInsertedRectangles.get(index);
		if (rectangle != null) {
			rectangle.x = inserted.x;
			rectangle.y = inserted.y;
			rectangle.width = inserted.width;
			rectangle.height = inserted.height;
			return rectangle;
		}

		return new Rectangle(inserted.x, inserted.y, inserted.width,
				inserted.height);
	}

	/**
	 * Gets the original id for the inserted rectangle in given index
	 * 
	 * @param index
	 * @return
	 */
	public int getRectangleId(int index) {
		Rectangle inserted = mInsertedRectangles.get(index);
		return inserted.id;
	}

	/**
	 * Add a rectangle to be packed into the packer
	 * 
	 * @width the width of inserted rectangle
	 * @height the height of inserted rectangle
	 * @id the identifier for this rectangle
	 * @return true if inserted successfully
	 */
	public void insertRectangle(double width, double height, int id) {
		SortableSize sortableSize = allocateSize(width, height, id);
		mInsertList.add(sortableSize);
	}

	/**
	 * Packs the rectangles inserted
	 * 
	 * @param sort
	 *            boolean defining whether to sort the inserted rectangles
	 *            before packing
	 * @return the number of the packed rectangles
	 */
	public int packRectangles(boolean sort) {

		while (mInsertList.size() > 0) {
			int lastIndex = mInsertList.size() - 1;
			SortableSize sortableSize = mInsertList.remove(lastIndex);
			double width = sortableSize.width();
			double height = sortableSize.height();

			int index = getFreeAreaIndex(width, height);
			if (index >= 0) {
				Rectangle freeArea = mFreeAreas.get(index);
				Rectangle target = allocateRectangle(freeArea.x, freeArea.y,
						width, height);
				target.id = sortableSize.id();

				// Generate the new free areas, these are parts of the old ones
				// intersected or touched by the target
				generateNewFreeAreas(target, mFreeAreas, mNewFreeAreas);

				while (mNewFreeAreas.size() > 0) {
					int lastFreeArea = mNewFreeAreas.size() - 1;
					Rectangle free = mNewFreeAreas.remove(lastFreeArea);
					mFreeAreas.add(free);
				}

				mInsertedRectangles.add(target);
				if ((target.x + target.width) > mPackedWidth) {
					mPackedWidth = target.x + target.width;
				}
				if ((target.y + target.height) > mPackedHeight) {
					mPackedHeight = (target.y + target.height);
				}
			}

			freeSize(sortableSize);
		}

		return getRectangleCount();
	}

	/**
	 * Removes rectangles from the filteredAreas that are sub rectangles of any
	 * rectangle in areas.
	 * 
	 * @param areas
	 *            rectangles from which the filtering is performed
	 */
	private void filterSelfSubAreas(ArrayList<Rectangle> areas) {
		for (int i = areas.size() - 1; i >= 0; i--) {
			Rectangle filtered = areas.get(i);
			for (int j = areas.size() - 1; j >= 0; j--) {
				if (i != j) {
					Rectangle area = areas.get(j);
					if (filtered.x >= area.x
							&& filtered.y >= area.y
							&& (filtered.x + filtered.width) <= (area.y + area.height)
							&& (filtered.y + filtered.height) <= (area.y + area.height)) {
						freeRectangle(filtered);
						int index = areas.size() - 1;
						Rectangle topOfStack = areas.remove(index);
						if (i < areas.size()) {
							// Move the one on the top to the freed position
							areas.add(i, topOfStack);
						}
						break;
					}
				}
			}
		}
	}

	/**
	 * Checks what areas the given rectangle intersects, removes those areas and
	 * returns the list of new areas those areas are divided into
	 * 
	 * @param target
	 *            the new rectangle that is dividing the areas
	 * @param areas
	 *            the areas to be divided
	 * @return list of new areas
	 */
	private void generateNewFreeAreas(Rectangle target,
			ArrayList<Rectangle> areas, ArrayList<Rectangle> results) {
		// Increase dimensions by one to get the areas on right / bottom this
		// rectangle touches
		// Also add the padding here
		double x = target.x;
		double y = target.y;
		double right = (target.x + target.width) + 1 + mPadding;
		double bottom = (target.y + target.height) + 1 + mPadding;

		Rectangle targetWithPadding = null;
		if (mPadding == 0) {
			targetWithPadding = target;
		}

		for (int i = areas.size() - 1; i >= 0; i--) {
			Rectangle area = areas.get(i);
			if (!(x >= (area.x + area.width) || right <= area.x
					|| y >= (area.y + area.height) || bottom <= area.y)) {
				if (targetWithPadding == null) {
					targetWithPadding = allocateRectangle(target.x, target.y,
							target.width + mPadding, target.height + mPadding);
				}

				generateDividedAreas(targetWithPadding, area, results);
				int lastArea = areas.size() - 1;
				Rectangle topOfStack = areas.remove(lastArea);
				if (i < areas.size()) {
					// Move the one on the top to the freed position
					areas.add(i, topOfStack);
				}
			}
		}

		if (targetWithPadding != null && targetWithPadding != target) {
			freeRectangle(targetWithPadding);
		}

		filterSelfSubAreas(results);
	}

	/**
	 * Divides the area into new sub areas around the divider.
	 * 
	 * @param divider
	 *            rectangle that intersects the area
	 * @param area
	 *            rectangle to be divided into sub areas around the divider
	 * @param results
	 *            vector for the new sub areas around the divider
	 */
	private void generateDividedAreas(Rectangle divider, Rectangle area,
			ArrayList<Rectangle> results) {
		int count = 0;
		double rightDelta = (area.x + area.width) - (divider.x + divider.width);
		if (rightDelta > 0) {
			results.add(allocateRectangle((divider.x + divider.width), area.y,
					rightDelta, area.height));
			count++;
		}

		double leftDelta = divider.x - area.x;
		if (leftDelta > 0) {
			results.add(allocateRectangle(area.x, area.y, leftDelta,
					area.height));
			count++;
		}

		double bottomDelta = (area.y + area.height)
				- (divider.y + divider.height);
		if (bottomDelta > 0) {
			results.add(allocateRectangle(area.x, (divider.y + divider.height),
					area.width, bottomDelta));
			count++;
		}

		double topDelta = divider.y - area.y;
		if (topDelta > 0) {
			results.add(allocateRectangle(area.x, area.y, area.width, topDelta));
			count++;
		}

		if (count == 0
				&& (divider.width < area.width || divider.height < area.height)) {
			// Only touching the area, store the area itself
			results.add(area);
		} else {
			freeRectangle(area);
		}
	}

	/**
	 * Gets the index of the best free area for the given rectangle
	 * 
	 * @width the width of inserted rectangle
	 * @height the height of inserted rectangle
	 * @return index of the best free area or -1 if no suitable free area
	 *         available
	 */
	private int getFreeAreaIndex(double width, double height) {
		Rectangle best = mOutsideRectangle;
		int index = -1;

		double paddedWidth = width + mPadding;
		double paddedHeight = height + mPadding;

		int count = mFreeAreas.size();
		for (int i = count - 1; i >= 0; i--) {
			Rectangle free = mFreeAreas.get(i);
			if (free.x < mPackedWidth || free.y < mPackedHeight) {
				// Within the packed area, padding required
				if (free.x < best.x && paddedWidth <= free.width
						&& paddedHeight <= free.height) {
					index = i;
					if ((paddedWidth == free.width && free.width <= free.height && (free.x + free.width) < mWidth)
							|| (paddedHeight == free.height && free.height <= free.width)) {
						break;
					}
					best = free;
				}
			} else {
				// Outside the current packed area, no padding required
				if (free.x < best.x && width <= free.width
						&& height <= free.height) {
					index = i;
					if ((width == free.width && free.width <= free.height && (free.x + free.width) < mWidth)
							|| (height == free.height && free.height <= free.width)) {
						break;
					}
					best = free;
				}
			}
		}

		return index;
	}

	/**
	 * Allocates new rectangle. If one available in stack uses that, otherwise
	 * new.
	 * 
	 * @param x
	 * @param y
	 * @param width
	 * @param height
	 * @return
	 */
	private Rectangle allocateRectangle(double x, double y, double width,
			double height) {
		if (mRectangleStack.size() > 0) {
			int index = mRectangleStack.size() - 1;
			Rectangle rectangle = mRectangleStack.remove(index);
			rectangle.x = x;
			rectangle.y = y;
			rectangle.width = width;
			rectangle.height = height;

			return rectangle;
		}

		return new Rectangle(x, y, width, height);
	}

	/**
	 * Pushes the freed rectangle to rectangle stack. Make sure not to push same
	 * rectangle twice!
	 * 
	 * @param rectangle
	 */
	private void freeRectangle(Rectangle rectangle) {
		mRectangleStack.add(rectangle);
	}

	/**
	 * Allocates new sortable size instance. If one available in stack uses
	 * that, otherwise new.
	 * 
	 * @param width
	 * @param height
	 * @param id
	 * @return
	 */
	private SortableSize allocateSize(double width, double height, int id) {
		if (mSortableSizeStack.size() > 0) {
			int index = mSortableSizeStack.size() - 1;
			SortableSize size = mSortableSizeStack.remove(index);
			size.setWidth(width);
			size.setHeight(height);
			size.setId(id);

			return size;
		}

		return new SortableSize(width, height, id);
	}

	/**
	 * Pushes the freed sortable size to size stack. Make sure not to push same
	 * size twice!
	 * 
	 * @param size
	 */
	private void freeSize(SortableSize size) {
		mSortableSizeStack.add(size);
	}
}
