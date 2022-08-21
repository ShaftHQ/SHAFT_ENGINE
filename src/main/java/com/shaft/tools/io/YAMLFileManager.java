package com.shaft.tools.io;

import com.shaft.tools.support.JavaActions;
import org.yaml.snakeyaml.Yaml;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Pattern;

public class YAMLFileManager {
    public final static String KEY_CONTAINS_LIST_REGEX = "^[-a-zA-Z\\d_!@#$%^&*()+=|\\\\/?><\"'{}~]*(\\[\\d+])+$";
    public final static String NUMBER_IN_SQUARE_BRACKETS_REGEX = "\\[\\d+]";
    public final static String SQUARE_BRACKETS_REGEX = "[\\[\\]]";
    public final static String KEY_SEPARATOR_REGEX = "\\.";

    private final Map<String, Object> data;
    private final String filePath;

    // Splitting the key to a list of keys using the dot as the splitter.
    private final Function<String, List<String>> splitKey =
            key -> Arrays.stream(key.split(KEY_SEPARATOR_REGEX)).toList();
    // Checking if the key has an index for a list [index]
    private final Predicate<String> keyHasList =
            key -> key.matches(KEY_CONTAINS_LIST_REGEX);

    /**
     * Creating an instance of {@link YAMLFileManager}
     *
     * @param filePath target test data yaml file path
     */
    public YAMLFileManager(String filePath) {
        this.filePath = JavaActions.appendTestDataToRelativePath(filePath);
        this.data = getData();
    }

    /**
     * Fetch all the date from the YAML file
     *
     * @return all date existed in the YAML file as {@link Map}
     */
    public Map<String, Object> get() {
        return data;
    }

    /**
     * Fetch a single piece of data from the YAML file using a single key or a series of keys
     * <p>
     * to support key series use a dot to separate keys "key1.key2"
     * to support list in the key series "key[index]"
     *
     * @param key the path to the wanted data can be a single key or a series of keys
     * @return the wanted value as {@link Object}
     */
    public Object get(String key) {
        if (key == null || key.isEmpty()) {
            var errorMsg = "Key can't be null or empty";
            sendFailureCompo(errorMsg);
            throw new RuntimeException(errorMsg);
        }

        Object value;
        var keys = splitKey.apply(key);

        if (keys.size() > 1)
            value = getValueFromKeys(data, keys);
        else if (keyHasList.test(key))
            value = getValueFromKeyList(data, key);
        else {
            if (data.containsKey(key))
                value = data.get(key);
            else {
                var errorMsg = "This key [" + key + "] is not exist";
                sendFailureCompo(errorMsg);
                throw new RuntimeException(errorMsg);
            }
        }

        return value;
    }

    /**
     * Fetch a single piece of data from the YAML file using a single key or a series of keys
     * <p>
     * to support key series use a dot to separate keys "key1.key2"
     * to support list in the key series "key[index]"
     *
     * @param key the path to the wanted data can be a single key or a series of keys
     * @return the wanted value as {@link String}
     */
    public String getString(String key) {
        return parseObjectTo(get(key), String.class);
    }

    /**
     * Fetch a single piece of data from the YAML file using a single key or a series of keys
     * <p>
     * to support key series use a dot to separate keys "key1.key2"
     * to support list in the key series "key[index]"
     *
     * @param key the path to the wanted data can be a single key or a series of keys
     * @return the wanted value as {@link Integer}
     */
    public Integer getInteger(String key) {
        return parseObjectTo(get(key), Integer.class);
    }

    /**
     * Fetch a single piece of data from the YAML file using a single key or a series of keys
     * <p>
     * to support key series use a dot to separate keys "key1.key2"
     * to support list in the key series "key[index]"
     *
     * @param key the path to the wanted data can be a single key or a series of keys
     * @return the wanted value as {@link Double}
     */
    public Double getDouble(String key) {
        return parseObjectTo(get(key), Double.class);
    }

    /**
     * Fetch a single piece of data from the YAML file using a single key or a series of keys
     * <p>
     * to support key series use a dot to separate keys "key1.key2"
     * to support list in the key series "key[index]"
     * <p>
     * to support {@link Long} values the char "L" should be added at the end of the number
     *
     * @param key the path to the wanted data can be a single key or a series of keys
     * @return the wanted value as {@link Long}
     */
    public Long getLong(String key) {
        String value;
        try {
            value = getString(key);
        } catch (ClassCastException ignore) {
            var errorMsg = "To support Long values please add 'L' at the end of the number";
            sendFailureCompo(errorMsg);
            throw new RuntimeException(errorMsg);
        }
        if (value.endsWith("L"))
            try {
                return Long.parseLong(value.replace("L", ""));
            } catch (NumberFormatException ignore) {
            }

        var errorMsg = "Can't parse the value of the key [" + key + "] to be long";
        sendFailureCompo(errorMsg);
        throw new NumberFormatException(errorMsg);
    }

    /**
     * Fetch a single piece of data from the YAML file using a single key or a series of keys
     * <p>
     * to support key series use a dot to separate keys "key1.key2"
     * to support list in the key series "key[index]"
     *
     * @param key the path to the wanted data can be a single key or a series of keys
     * @return the wanted value as {@link Boolean}
     */
    public Boolean getBoolean(String key) {
        return parseObjectTo(get(key), Boolean.class);
    }

    /**
     * Fetch a single piece of data from the YAML file using a single key or a series of keys
     * <p>
     * to support key series use a dot to separate keys "key1.key2"
     * to support list in the key series "key[index]"
     * <p>
     * Always return date on the local time zone
     *
     * @param key the path to the wanted data can be a single key or a series of keys
     * @return the wanted value as {@link Date}
     */
    public Date getDate(String key) {
        return parseObjectTo(get(key), Date.class);
    }

    /**
     * Fetch a single piece of data from the YAML file using a single key or a series of keys
     * <p>
     * to support key series use a dot to separate keys "key1.key2"
     * to support list in the key series "key[index]"
     *
     * @param key   the path to the wanted data can be a single key or a series of keys
     * @param clazz the class that data wanted to be parsed for
     * @return the wanted value as {@link T}
     */
    public <T> T getAs(String key, Class<T> clazz) {
        return parseObjectTo(get(key), clazz);
    }

    /**
     * Fetch a single piece of data from the YAML file using a single key or a series of keys
     * <p>
     * to support key series use a dot to separate keys "key1.key2"
     * to support list in the key series "key[index]"
     *
     * @param key   the path to the wanted data can be a single key or a series of keys
     * @param clazz the class that data wanted to be parsed for
     * @return the wanted value as {@link List<T>}
     */
    public <T> List<T> getListAs(String key, Class<T> clazz) {
        return parseObjectToList(get(key), clazz);
    }

    /**
     * Fetch a single piece of data from the YAML file using a single key or a series of keys
     * <p>
     * to support key series use a dot to separate keys "key1.key2"
     * to support list in the key series "key[index]"
     *
     * @param key   the path to the wanted data can be a single key or a series of keys
     * @param clazz the class that data wanted to be parsed for
     * @return the wanted value as {@link Map} of {@link String} and {@link T}
     */
    public <T> Map<String, T> getMapAs(String key, Class<T> clazz) {
        return parseObjectToMap(get(key), clazz);
    }

    /**
     * Used internally to fetch a value form a map using a list of keys
     *
     * @param map the map wanted to fetch data form
     * @param keys the list of keys that point on the value
     * @return the wanted value as {@link Object}
     */
    private Object getValueFromKeys(Map<String, Object> map, List<String> keys) {
        Object value = map;
        for (int i = 0; i < keys.size(); i++) {
            var isLastItem = i == keys.size() - 1;
            if (keyHasList.test(keys.get(i))) {
                value = getValueFromKeyList(
                        parseObjectToMap(value, Object.class),
                        keys.get(i));

                if (isLastItem) break;

            } else
                value = parseObjectToMap(value, Object.class)
                        .get(keys.get(i));

            if (!isLastItem)
                value = parseObjectToMap(value, Object.class);
        }

        return value;
    }

    /**
     * Used internally to fetch data from keys that have an index for a list in it
     * @param map the map wanted to fetch data form
     * @param key the key that has the index on it
     * @return the wanted value as {@link Object}
     */
    private Object getValueFromKeyList(Map<String, Object> map, String key) {
        var indexes = parseKeyList(key);
        key = key.replaceAll(NUMBER_IN_SQUARE_BRACKETS_REGEX, "");

        var value = map.get(key);

        for (int i = 0; i < indexes.size(); i++) {
            value = parseObjectToList(value, Object.class)
                    .get(indexes.stream().findFirst().orElseThrow());
        }

        return value;
    }

    /**
     * Used internally to fetch all data existed in the YAML file
     * @return all data in the YAML file as {@link Map} of {@link String} and {@link Object}
     */
    private Map<String, Object> getData() {
        var file = this.getFile();
        Map<String, Object> loadedData = new Yaml().load(file);

        this.closeFile(file);

        return loadedData;
    }

    /**
     * Used internally to load the wanted YAML file
     * @return {@link FileInputStream} instance of the desired file
     */
    private FileInputStream getFile() {
        FileInputStream in;

        try {
            in = new FileInputStream(filePath);
        } catch (FileNotFoundException rootCauseException) {
            var errorMsg = "Couldn't find the desired file. [" + filePath + "].";
            sendFailureCompo(errorMsg, rootCauseException);
            throw new RuntimeException(errorMsg);
        }
        return in;
    }

    /**
     * Used internally to close the opened file
     * @param file the file wanted to be closed
     */
    private void closeFile(FileInputStream file) {
        try {
            file.close();
        } catch (IOException e) {
            throw new RuntimeException("Couldn't close the following file. [" + filePath + "] with exception:" + e);
        }
    }

    /**
     * Used internally to fetch the index or indexes from the key that support list
     * @param key the key that contains one or more index
     * @return a {@link List<Integer>} that contains one or more index
     */
    private List<Integer> parseKeyList(String key) {
        var indexes = new ArrayList<Integer>();

        var matcher = Pattern
                .compile(NUMBER_IN_SQUARE_BRACKETS_REGEX).matcher(key);

        while (matcher.find()) {
            indexes.add(Integer.parseInt(
                    matcher
                            .group()
                            .replaceAll(SQUARE_BRACKETS_REGEX, "")
            ));
        }

        return indexes;
    }

    /**
     * Used internally to parse {@link Object} to be any type
     *
     * @param obj the value wanted to be parsed
     * @param clazz the class that data wanted to be parsed for
     * @return the value as {@link T}
     * @param <T> the wanted type
     */
    private <T> T parseObjectTo(Object obj, Class<T> clazz) {
        T v;
        try {
            v = clazz.cast(obj);
        } catch (ClassCastException rootCauseException) {
            var errorMsg = "Can't parse the value of [" + obj + "] to be of type [" + clazz.getSimpleName() + "]";
            sendFailureCompo(errorMsg, rootCauseException);
            throw rootCauseException;
        }
        return v;
    }

    /**
     * Used internally to parse {@link Object} to be a {@link List<T>}
     *
     * @param obj the value wanted to be parsed
     * @param clazz the class that data wanted to be parsed for
     * @return the value as {@link List<T>}
     * @param <T> the wanted type
     */
    private <T> List<T> parseObjectToList(Object obj, Class<T> clazz) {
        if (obj instanceof List<?> list) {
            return list.stream()
                    .map(item -> parseObjectTo(item, clazz)).toList();
        }

        var errorMsg = "Can't parse the value of [" + obj + "] to be list";
        sendFailureCompo(errorMsg);
        throw new RuntimeException(errorMsg);
    }

    /**
     * Used internally to parse {@link Object} to be a {@link Map} of {@link String} and {@link T}
     *
     * @param obj the value wanted to be parsed
     * @param clazz the class that data wanted to be parsed for
     * @return the value as {@link Map} of {@link String} and {@link T}
     * @param <T> the wanted type
     */
    private <T> Map<String, T> parseObjectToMap(Object obj, Class<T> clazz) {
        if (obj instanceof Map<?, ?> map) {
            var nMap = new HashMap<String, T>();
            map.forEach(
                    (k, v) -> nMap.put(k.toString(), parseObjectTo(v, clazz))
            );
            return nMap;
        }
        var errorMsg = "Can't parse the value of [" + obj + "] to be map";
        sendFailureCompo(errorMsg);
        throw new RuntimeException(errorMsg);
    }

    private void sendFailureCompo(String msg, Exception... rootCauseException) {
        if (rootCauseException.length > 0)
            ReportManagerHelper.log(rootCauseException[0]);

        ReportManager.log(msg);
    }
}
