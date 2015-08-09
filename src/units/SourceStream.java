package units;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class SourceStream {

    private final List<Unit> fullStream;
    private final List<Unit> remainingStream;
    private final int seed;

    public static List<SourceStream> getSourceStreams(JSONObject json) {
        JSONArray jsonUnits = json.getJSONArray("units");
        List<Unit> baseUnits = Unit.fromJson(jsonUnits);
        int streamLength = json.getInt("sourceLength");
        JSONArray seeds = json.getJSONArray("sourceSeeds");
        List<SourceStream> streams = new ArrayList<>();
        for (int i = 0; i < seeds.length(); i++) {
            int seed = seeds.getInt(i);
            streams.add(new SourceStream(streamLength, seed, baseUnits));
        }
        return streams;
    }

    public SourceStream(int sourceLength, int seed, List<Unit> baseUnits) {
        this.seed = seed;
        int mod = baseUnits.size();
        int[] longIndexes = RandomGenerator.getNumbers(seed, sourceLength);
        fullStream = new ArrayList<>();
        for (int baseindex : longIndexes) {
            int unitindex = baseindex % mod;
            fullStream.add(baseUnits.get(unitindex));
        }
        remainingStream = new ArrayList<>(fullStream);
    }
    
    private SourceStream(int seed, List<Unit> fullStream, List<Unit> remainingStream) {
    	this.seed = seed;
    	this.fullStream = new ArrayList<>(fullStream);
    	this.remainingStream = new ArrayList<>(remainingStream);
    }
    
    public SourceStream getCopy() {
    	return new SourceStream(seed, fullStream, remainingStream);
    }

    public Unit popNextUnit() {
        Unit unit = remainingStream.get(0);
        remainingStream.remove(0);
        return unit;
    }

    public Unit seeNextUnit() {
        return remainingStream.get(0);
    }

    public List<Unit> getFullStream() {
        return new ArrayList<>(fullStream);
    }

    public List<Unit> getRemainingStream() {
        return new ArrayList<>(remainingStream);
    }

    public int getRemainingCount() {
        return remainingStream.size();
    }

    public int getTotalCount() {
        return fullStream.size();
    }

    public int getSeed() {
        return seed;
    }
}
