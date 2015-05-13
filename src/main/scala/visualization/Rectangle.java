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
	public double x;
	
	/**
	 * 
	 */
	public double y;
	
	/**
	 * 
	 */
	public double width;
	
	/**
	 * 
	 */
	public double height;
	
	/**
	 * 
	 */
	public double right;
	
	/**
	 * 
	 */
	public double bottom;
	
	public int id;
	
	public Rectangle(double x, double y, double width, double height) {
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
		this.right = x + width;
		this.bottom = y + height;
	}
	
	public Rectangle(Rectangle r) {
		this.x = r.x;
		this.y = r.y;
		this.width = r.width;
		this.height = r.height;
		this.right = r.right;
		this.bottom = r.bottom;
	}
	
	public void setX(double x) {
		this.x = x;
	}
	
	public void setY(double y) {
		this.y = y;
	}
	
	public void setWidth(double width) {
		this.width = width;
	}
	
	public void setHeight(double height) {
		this.height = height;
	}
	
	@Override
	public String toString() {
		return "[ " + x + ", " + y + ", " + width + ", " + height + " ]";
	}
}