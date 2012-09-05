package org.freedesktop;
import java.util.List;
import java.util.Map;
import org.freedesktop.dbus.DBusInterface;
import org.freedesktop.dbus.UInt32;
import org.freedesktop.dbus.Variant;
public interface Notifications extends DBusInterface
{

  public Quad<String, String, String, String> GetServerInformation();
  public List<String> GetCapabilities();
  public void CloseNotification(UInt32 id);
  public UInt32 Notify(String app_name, UInt32 id, String icon, String summary, String body, List<String> actions, Map<String,Variant> hints, int timeout);

}
