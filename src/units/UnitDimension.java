package units;

public class UnitDimension {

    public final int minY;
    public final int maxY;
    public final int minX;
    public final int maxX;
    
    public UnitDimension(int minX, int maxX, int minY, int maxY) {
        this.minX = minX;
        this.maxX = maxX;
        this.minY = minY;
        this.maxY = maxY;
    }
    
}
