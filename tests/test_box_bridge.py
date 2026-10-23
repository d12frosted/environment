"""Tests for the pure parts of bin/box-bridge (run: python3 -m unittest discover tests)."""

import importlib.machinery
import importlib.util
import os
import socket
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
SCRIPT = os.path.join(HERE, "..", "bin", "box-bridge")

loader = importlib.machinery.SourceFileLoader("box_bridge", SCRIPT)
spec = importlib.util.spec_from_loader("box_bridge", loader)
box_bridge = importlib.util.module_from_spec(spec)
loader.exec_module(box_bridge)


class LocalhostPortsTest(unittest.TestCase):
    def ports(self, url):
        return box_bridge.localhost_ports(url)

    def test_redirect_uri_with_localhost(self):
        url = "https://wix-bo.com/dev/dpx/authenticate?redirect_uri=http%3A%2F%2Flocalhost%3A41234%2Fcallback"
        self.assertEqual(self.ports(url), [41234])

    def test_redirect_uri_with_loopback_ip(self):
        url = "https://auth.example.com/authorize?client_id=x&redirect_uri=http%3A%2F%2F127.0.0.1%3A5555%2Fcallback&state=y"
        self.assertEqual(self.ports(url), [5555])

    def test_double_encoded_redirect(self):
        url = "https://sso.example.com/login?next=%2Fauthorize%3Fredirect_uri%3Dhttp%253A%252F%252Flocalhost%253A6001%252Fcb"
        self.assertEqual(self.ports(url), [6001])

    def test_url_on_localhost_itself(self):
        self.assertEqual(self.ports("http://localhost:3000/preview"), [3000])

    def test_ipv6_loopback(self):
        self.assertEqual(self.ports("https://a.example.com/?cb=http://[::1]:7007/x"), [7007])

    def test_no_localhost(self):
        self.assertEqual(self.ports("https://example.com/?redirect_uri=https://example.org/cb"), [])

    def test_localhost_without_port(self):
        self.assertEqual(self.ports("https://a.example.com/?redirect_uri=http://localhost/cb"), [])

    def test_privileged_and_invalid_ports_are_ignored(self):
        self.assertEqual(self.ports("https://a.example.com/?a=http://localhost:80/&b=http://localhost:99999/"), [])

    def test_lookalike_host_is_ignored(self):
        self.assertEqual(self.ports("https://a.example.com/?r=http://notlocalhost:4000/"), [])

    def test_duplicates_collapse(self):
        url = "http://localhost:4000/?redirect_uri=http%3A%2F%2Flocalhost%3A4000%2Fcb"
        self.assertEqual(self.ports(url), [4000])


class ConfigTest(unittest.TestCase):
    def parse(self, text):
        with tempfile.NamedTemporaryFile("w", delete=False) as f:
            f.write(text)
        try:
            return box_bridge.load_config(f.name)
        finally:
            os.unlink(f.name)

    def test_full_config(self):
        config = self.parse(
            "# the box\n"
            "host coder.my-box\n"
            "\n"
            "forward 37999\n"
            "forward 48080 8080   # laptop port differs\n"
            "ssh_option ProxyCommand=coder ssh --stdio my-box\n"
        )
        self.assertEqual(config.host, "coder.my-box")
        self.assertEqual(config.forwards, [(37999, 37999), (48080, 8080)])
        self.assertEqual(config.ssh_options, ["ProxyCommand=coder ssh --stdio my-box"])

    def test_missing_host(self):
        with self.assertRaises(box_bridge.ConfigError):
            self.parse("forward 37999\n")

    def test_bad_port(self):
        with self.assertRaises(box_bridge.ConfigError):
            self.parse("host b\nforward abc\n")

    def test_unknown_key(self):
        with self.assertRaises(box_bridge.ConfigError):
            self.parse("host b\nfrobnicate 1\n")


class PortBusyTest(unittest.TestCase):
    def test_bound_port_is_busy(self):
        with socket.socket() as s:
            s.bind(("127.0.0.1", 0))
            s.listen()
            self.assertTrue(box_bridge.port_busy(s.getsockname()[1]))

    def test_free_port_is_not_busy(self):
        with socket.socket() as s:
            s.bind(("127.0.0.1", 0))
            port = s.getsockname()[1]
        self.assertFalse(box_bridge.port_busy(port))


class FakeConnection(box_bridge.Connection):
    """Connection with ssh replaced by a log of mux calls."""

    def __init__(self, forwards=(), busy=()):
        config = box_bridge.Config()
        config.host = "box"
        config.forwards = list(forwards)
        super().__init__(config)
        self.calls = []
        self.busy = set(busy)

    def forward(self, local, remote):
        self.calls.append(("forward", local))
        return None

    def cancel(self, local, remote):
        self.calls.append(("cancel", local))

    def port_busy(self, port):
        return port in self.busy


class DynamicForwardTest(unittest.TestCase):
    def tearDown(self):
        self.connection.stop()

    def test_forward_then_close(self):
        self.connection = FakeConnection()
        self.assertEqual(self.connection.forward_for_url(5000)[0], "added")
        self.assertTrue(self.connection.close(5000))
        self.assertEqual(self.connection.calls, [("forward", 5000), ("cancel", 5000)])
        self.assertFalse(self.connection.close(5000))

    def test_already_forwarded_port_is_not_added_again(self):
        self.connection = FakeConnection()
        self.connection.forward_for_url(5000)
        self.assertEqual(self.connection.forward_for_url(5000)[0], "exists")
        self.assertEqual(self.connection.calls, [("forward", 5000)])

    def test_port_bound_on_the_laptop_is_refused(self):
        self.connection = FakeConnection(busy=[5000])
        status, message = self.connection.forward_for_url(5000)
        self.assertEqual(status, "error")
        self.assertIn("busy on the laptop", message)
        self.assertEqual(self.connection.calls, [])

    def test_static_forward_is_never_added_or_closed(self):
        self.connection = FakeConnection(forwards=[(37999, 37999)], busy=[37999])
        self.assertEqual(self.connection.forward_for_url(37999)[0], "static")
        self.assertFalse(self.connection.close(37999))
        self.assertEqual(self.connection.calls, [])

    def test_ttl_drops_the_forward(self):
        self.connection = FakeConnection()
        self.connection.ttl = 0.1
        self.connection.forward_for_url(5000)
        import time
        time.sleep(0.3)
        self.assertEqual(self.connection.calls, [("forward", 5000), ("cancel", 5000)])
        self.assertFalse(self.connection.close(5000))


if __name__ == "__main__":
    unittest.main()
