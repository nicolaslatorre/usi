package visualization;

/**
 * Yet another Rectangle class. Only here to remove dependencies on
 * awt/lwjgl/etc
 * 
 * @author ryanm
 */
public class Rectangle {
	/**
	 * 
	 */
	public final int x;
	
	/**
	 * 
	 */
	public final int y;
	
	/**
	 * 
	 */
	public final int width;
	
	/**
	 * 
	 */
	public final int height;
	
	public Rectangle(int x, int y, int width, int height) {
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
	}
	
	public Rectangle(Rectangle r) {
		this.x = r.x;
		this.y = r.y;
		this.width = r.width;
		this.height = r.height;
	}
	
	@Override
	public String toString() {
		return "[ " + x + ", " + y + ", " + width + ", " + height + " ]";
	}
}