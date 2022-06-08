# Aono

莫名其妙的静态博客，非markdown博客。

# 特色

最大的特色就是没特色，我的笔记都是pdf，该程序只要指定任意目录，就能在同级目录下生成静态文件。

该程序不会处理任何文本信息，包括markdown、org……

# 使用

```bash
$ aono <指定任何目录>
```

之后就会得到一个静态网页（index.html）和一个样式表（aono.css），并且没有javascript（很垃圾）。双击即可访问。

# 从源码编译

这个版本使用cabal，摒弃了stack，编译起来比较轻松的：

```bash
$ cabal build
```

也可以安装到用户全局环境：

```
$ cabal install
```

# 发布协议

AGPL v3
